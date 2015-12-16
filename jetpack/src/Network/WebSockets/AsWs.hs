module Network.WebSockets.AsWs
  ( -- unqualified class re-export
  I.WebSocketsData(I.fromLazyByteString, I.toLazyByteString)
  , module Network.WebSockets.AsWs
  ) where
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
get_ws_acceptSubprotocol o = I.acceptSubprotocol o
set_ws_acceptSubprotocol x o = o { I.acceptSubprotocol = x}

-- constructor :: Maybe ByteString -> AcceptRequest
ws_mk'AcceptRequest =  I.AcceptRequest
pattern WsAcceptRequest a <-  I.AcceptRequest a

type WsConnection  = I.Connection

type WsConnectionOptions  = I.ConnectionOptions
get_ws_connectionOnPong o = I.connectionOnPong o
set_ws_connectionOnPong x o = o { I.connectionOnPong = x}

-- constructor :: IO () -> ConnectionOptions
ws_mk'ConnectionOptions =  I.ConnectionOptions
pattern WsConnectionOptions a <-  I.ConnectionOptions a

type WsPendingConnection  = I.PendingConnection
get_ws_pendingRequest o = I.pendingRequest o
set_ws_pendingRequest x o = o { I.pendingRequest = x}

type WsHandshakeException  = I.HandshakeException

-- constructor :: NotSupported
ws_mk'NotSupported =  I.NotSupported
pattern WsNotSupported  <-  I.NotSupported 

-- constructor :: RequestHead -> String -> MalformedRequest
ws_mk'MalformedRequest =  I.MalformedRequest
pattern WsMalformedRequest a b <-  I.MalformedRequest a b

-- constructor :: ResponseHead -> String -> MalformedResponse
ws_mk'MalformedResponse =  I.MalformedResponse
pattern WsMalformedResponse a b <-  I.MalformedResponse a b

-- constructor :: Request -> String -> RequestRejected
ws_mk'RequestRejected =  I.RequestRejected
pattern WsRequestRejected a b <-  I.RequestRejected a b

-- constructor :: String -> OtherHandshakeException
ws_mk'OtherHandshakeException =  I.OtherHandshakeException
pattern WsOtherHandshakeException a <-  I.OtherHandshakeException a

type WsHeaders  = I.Headers

type WsRequest  = I.Request

-- constructor :: RequestHead -> ByteString -> Request
ws_mk'Request =  I.Request
pattern WsRequest a b <-  I.Request a b

type WsRequestHead  = I.RequestHead
get_ws_requestPath o = I.requestPath o
set_ws_requestPath x o = o { I.requestPath = x}
get_ws_requestHeaders o = I.requestHeaders o
set_ws_requestHeaders x o = o { I.requestHeaders = x}
get_ws_requestSecure o = I.requestSecure o
set_ws_requestSecure x o = o { I.requestSecure = x}

-- constructor :: ByteString -> Headers -> Bool -> RequestHead
ws_mk'RequestHead =  I.RequestHead
pattern WsRequestHead a b c <-  I.RequestHead a b c

type WsResponse  = I.Response

-- constructor :: ResponseHead -> ByteString -> Response
ws_mk'Response =  I.Response
pattern WsResponse a b <-  I.Response a b

type WsResponseHead  = I.ResponseHead
get_ws_responseCode o = I.responseCode o
set_ws_responseCode x o = o { I.responseCode = x}
get_ws_responseMessage o = I.responseMessage o
set_ws_responseMessage x o = o { I.responseMessage = x}
get_ws_responseHeaders o = I.responseHeaders o
set_ws_responseHeaders x o = o { I.responseHeaders = x}

-- constructor :: Int -> ByteString -> Headers -> ResponseHead
ws_mk'ResponseHead =  I.ResponseHead
pattern WsResponseHead a b c <-  I.ResponseHead a b c

type WsServerApp  = I.ServerApp

type WsConnectionException  = I.ConnectionException

-- constructor :: Word16 -> ByteString -> CloseRequest
ws_mk'CloseRequest =  I.CloseRequest
pattern WsCloseRequest a b <-  I.CloseRequest a b

-- constructor :: ConnectionClosed
ws_mk'ConnectionClosed =  I.ConnectionClosed
pattern WsConnectionClosed  <-  I.ConnectionClosed 

-- constructor :: String -> ParseException
ws_mk'ParseException =  I.ParseException
pattern WsParseException a <-  I.ParseException a

type WsControlMessage  = I.ControlMessage

-- constructor :: Word16 -> ByteString -> Close
ws_mk'Close =  I.Close
pattern WsClose a b <-  I.Close a b

-- constructor :: ByteString -> Ping
ws_mk'Ping =  I.Ping
pattern WsPing a <-  I.Ping a

-- constructor :: ByteString -> Pong
ws_mk'Pong =  I.Pong
pattern WsPong a <-  I.Pong a

type WsDataMessage  = I.DataMessage

-- constructor :: ByteString -> Text
ws_mk'Text =  I.Text
pattern WsText a <-  I.Text a

-- constructor :: ByteString -> Binary
ws_mk'Binary =  I.Binary
pattern WsBinary a <-  I.Binary a

type WsMessage  = I.Message

-- constructor :: ControlMessage -> ControlMessage
ws_mk'ControlMessage =  I.ControlMessage
pattern WsControlMessage a <-  I.ControlMessage a

-- constructor :: DataMessage -> DataMessage
ws_mk'DataMessage =  I.DataMessage
pattern WsDataMessage a <-  I.DataMessage a
