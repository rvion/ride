module Network.WebSockets.AsWs where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Network.WebSockets as I

-- ws_runClient :: forall a. String -> Int -> String -> ClientApp a -> IO a
ws_runClient = I.runClient

-- ws_runClientWith :: forall a. String -> Int -> String -> ConnectionOptions -> Headers -> ClientApp a -> IO a
ws_runClientWith = I.runClientWith

-- ws_runClientWithSocket :: forall a. Socket -> String -> String -> ConnectionOptions -> Headers -> ClientApp a -> IO a
ws_runClientWithSocket = I.runClientWithSocket

-- ws_runClientWithStream :: forall a. Stream -> String -> String -> ConnectionOptions -> Headers -> ClientApp a -> IO a
ws_runClientWithStream = I.runClientWithStream

-- ws_acceptRequest :: PendingConnection -> IO Connection
ws_acceptRequest = I.acceptRequest

-- ws_acceptRequestWith :: PendingConnection -> AcceptRequest -> IO Connection
ws_acceptRequestWith = I.acceptRequestWith

-- ws_defaultConnectionOptions :: ConnectionOptions
ws_defaultConnectionOptions = I.defaultConnectionOptions

-- ws_forkPingThread :: Connection -> Int -> IO ()
ws_forkPingThread = I.forkPingThread

-- ws_receive :: Connection -> IO Message
ws_receive = I.receive

-- ws_receiveData :: forall a. WebSocketsData a => Connection -> IO a
ws_receiveData = I.receiveData

-- ws_receiveDataMessage :: Connection -> IO DataMessage
ws_receiveDataMessage = I.receiveDataMessage

-- ws_rejectRequest :: PendingConnection -> ByteString -> IO ()
ws_rejectRequest = I.rejectRequest

-- ws_send :: Connection -> Message -> IO ()
ws_send = I.send

-- ws_sendBinaryData :: forall a. WebSocketsData a => Connection -> a -> IO ()
ws_sendBinaryData = I.sendBinaryData

-- ws_sendClose :: forall a. WebSocketsData a => Connection -> a -> IO ()
ws_sendClose = I.sendClose

-- ws_sendDataMessage :: Connection -> DataMessage -> IO ()
ws_sendDataMessage = I.sendDataMessage

-- ws_sendPing :: forall a. WebSocketsData a => Connection -> a -> IO ()
ws_sendPing = I.sendPing

-- ws_sendTextData :: forall a. WebSocketsData a => Connection -> a -> IO ()
ws_sendTextData = I.sendTextData

-- ws_getRequestSubprotocols :: RequestHead -> [ByteString]
ws_getRequestSubprotocols = I.getRequestSubprotocols

-- ws_makeListenSocket :: String -> Int -> IO Socket
ws_makeListenSocket = I.makeListenSocket

-- ws_makePendingConnection :: Socket -> ConnectionOptions -> IO PendingConnection
ws_makePendingConnection = I.makePendingConnection

-- ws_makePendingConnectionFromStream :: Stream -> ConnectionOptions -> IO PendingConnection
ws_makePendingConnectionFromStream = I.makePendingConnectionFromStream

-- ws_runServer :: String -> Int -> ServerApp -> IO ()
ws_runServer = I.runServer

-- ws_runServerWith :: String -> Int -> ConnectionOptions -> ServerApp -> IO ()
ws_runServerWith = I.runServerWith

type WsClientApp a = I.ClientApp a
type WsAcceptRequest  = I.AcceptRequest
type WsConnection  = I.Connection
type WsConnectionOptions  = I.ConnectionOptions
type WsPendingConnection  = I.PendingConnection
type WsHandshakeException  = I.HandshakeException
type WsHeaders  = I.Headers
type WsRequest  = I.Request
type WsRequestHead  = I.RequestHead
type WsResponse  = I.Response
type WsResponseHead  = I.ResponseHead
type WsServerApp  = I.ServerApp
type WsConnectionException  = I.ConnectionException
type WsControlMessage  = I.ControlMessage
type WsDataMessage  = I.DataMessage
type WsMessage  = I.Message
