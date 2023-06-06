-- | Rate limit the number of requests to the OpenAI API according to data returned in the request.
--
module OpenAI.Client.RateLimit
  (rateLimitManager)
where
import Network.HTTP.Client
import Data.Time.Clock
import Control.Monad(when)
import Control.Concurrent
import Network.HTTP.Types.Status(Status,statusCode)
import Data.IORef(IORef,atomicModifyIORef',newIORef,readIORef)

requestIsToOpenAI :: Request -> Bool
requestIsToOpenAI req = host req ==  "api.openai.com"

rateLimitManager :: ManagerSettings -> IO ManagerSettings
rateLimitManager settings = do
  -- Begin with no barrier and an initial delay of 0 seconds.
  -- we impose a minimum delay on 429 responses of 4 seconds.
  let initial = (UTCTime (toEnum 0) 0,0)
  waitForRateLimit <- newIORef initial
  pure $ settings
    { managerModifyRequest = \req -> do
        when (requestIsToOpenAI req) (waitForSlot waitForRateLimit)
        pure req
    , managerModifyResponse = \resp -> do
        when (requestIsToOpenAI $ getOriginalRequest resp) (releaseSlot waitForRateLimit $ responseStatus resp)
        pure resp
    }

waitForSlot :: IORef ((UTCTime, NominalDiffTime)) -> IO ()
waitForSlot slot = do
  now <- getCurrentTime
  (barrier, _) <- readIORef slot
  when (barrier > now) $ do
    threadDelay $ ceiling $ 1000000 * (diffUTCTime barrier now)
    waitForSlot slot

releaseSlot :: IORef (UTCTime,NominalDiffTime) -> Status -> IO ()
releaseSlot slot status = do
  now <- getCurrentTime
  atomicModifyIORef' slot $ \(_,delay) ->
    let newDelay = case statusCode status of
          429 -> max (2*delay) (secondsToNominalDiffTime 4)
          200 -> 0.5*delay
          -- no real go/no-go decision here, so no change.
          _ -> delay
    in ((addUTCTime newDelay now, newDelay), ())
