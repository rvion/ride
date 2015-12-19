module Network.HTTP.Conduit.AsHttp
  ( module Network.HTTP.Conduit.AsHttp
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Network.HTTP.Conduit as I


-- http_http :: forall (m :: * -> *). MonadResource m => Request -> Manager -> m (Response (ResumableSource m ByteString))
http_http = I.http

-- http_lbsResponse :: forall (m :: * -> *). Monad m => Response (ResumableSource m ByteString) -> m (Response ByteString)
http_lbsResponse = I.lbsResponse

-- http_requestBodySource :: Int64 -> Source (ResourceT IO) ByteString -> RequestBody
http_requestBodySource = I.requestBodySource

-- http_requestBodySourceChunked :: Source (ResourceT IO) ByteString -> RequestBody
http_requestBodySourceChunked = I.requestBodySourceChunked

-- http_requestBodySourceChunkedIO :: Source IO ByteString -> RequestBody
http_requestBodySourceChunkedIO = I.requestBodySourceChunkedIO

-- http_requestBodySourceIO :: Int64 -> Source IO ByteString -> RequestBody
http_requestBodySourceIO = I.requestBodySourceIO

-- http_simpleHttp :: forall (m :: * -> *). MonadIO m => String -> m ByteString
http_simpleHttp = I.simpleHttp

-- http_mkManagerSettings :: TLSSettings -> Maybe SockSettings -> ManagerSettings
http_mkManagerSettings = I.mkManagerSettings

-- http_tlsManagerSettings :: ManagerSettings
http_tlsManagerSettings = I.tlsManagerSettings

-- http_addProxy :: ByteString -> Int -> Request -> Request
http_addProxy = I.addProxy

-- http_alwaysDecompress :: ByteString -> Bool
http_alwaysDecompress = I.alwaysDecompress

-- http_browserDecompress :: ByteString -> Bool
http_browserDecompress = I.browserDecompress

-- http_getRedirectedRequest :: Request -> ResponseHeaders -> CookieJar -> Int -> Maybe Request
http_getRedirectedRequest = I.getRedirectedRequest
