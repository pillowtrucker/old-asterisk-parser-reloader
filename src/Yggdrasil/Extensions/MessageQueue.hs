{-# LANGUAGE OverloadedStrings #-}
module Yggdrasil.Extensions.MessageQueue where
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.AMQP
import Yggdrasil.Types
import Yggdrasil.Util ()
import Prelude hiding (putStrLn,(++))
import qualified Data.Text as T
(++) :: BL.ByteString -> BL.ByteString -> BL.ByteString
a ++ b = BL.append a b

theQueueName :: T.Text
theQueueName = "reloads"
theExchangeName :: T.Text
theExchangeName = "rExchange"
theChanKey :: T.Text
theChanKey = "reloads"



initMq :: MyConfig T.Text -> IO (Connection, Channel)
initMq c = do
    conn <- openConnection (T.unpack $ mqHost c) (mqVhost c) (mqUser c) (mqPass c)
    chan <- openChannel conn
    declareExchange chan newExchange {exchangeName = theExchangeName, exchangeType = "fanout"}

    publishMsg chan theExchangeName theChanKey
        newMsg {msgBody = (BL.pack "Initialised publisher."),
                msgDeliveryMode = Just NonPersistent}
    return (conn,chan)

extReloadCallback :: (Message,Envelope) -> IO ()
extReloadCallback (msg, env) = do
    BL.putStrLn $ "received message: " ++ (msgBody msg)
    ackEnv env
