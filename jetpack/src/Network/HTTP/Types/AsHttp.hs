module Network.HTTP.Types.AsHttp
  ( -- unqualified class re-export
  I.QueryLike(I.toQuery)
  , module Network.HTTP.Types.AsHttp
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Network.HTTP.Types as I


-- http_hAccept :: HeaderName
http_hAccept = I.hAccept

-- http_hAcceptLanguage :: HeaderName
http_hAcceptLanguage = I.hAcceptLanguage

-- http_hAuthorization :: HeaderName
http_hAuthorization = I.hAuthorization

-- http_hCacheControl :: HeaderName
http_hCacheControl = I.hCacheControl

-- http_hConnection :: HeaderName
http_hConnection = I.hConnection

-- http_hContentEncoding :: HeaderName
http_hContentEncoding = I.hContentEncoding

-- http_hContentLength :: HeaderName
http_hContentLength = I.hContentLength

-- http_hContentMD5 :: HeaderName
http_hContentMD5 = I.hContentMD5

-- http_hContentType :: HeaderName
http_hContentType = I.hContentType

-- http_hCookie :: HeaderName
http_hCookie = I.hCookie

-- http_hDate :: HeaderName
http_hDate = I.hDate

-- http_hIfModifiedSince :: HeaderName
http_hIfModifiedSince = I.hIfModifiedSince

-- http_hIfRange :: HeaderName
http_hIfRange = I.hIfRange

-- http_hLastModified :: HeaderName
http_hLastModified = I.hLastModified

-- http_hLocation :: HeaderName
http_hLocation = I.hLocation

-- http_hRange :: HeaderName
http_hRange = I.hRange

-- http_hReferer :: HeaderName
http_hReferer = I.hReferer

-- http_hServer :: HeaderName
http_hServer = I.hServer

-- http_hUserAgent :: HeaderName
http_hUserAgent = I.hUserAgent

-- http_renderByteRange :: ByteRange -> ByteString
http_renderByteRange = I.renderByteRange

-- http_renderByteRangeBuilder :: ByteRange -> Builder
http_renderByteRangeBuilder = I.renderByteRangeBuilder

-- http_renderByteRanges :: ByteRanges -> ByteString
http_renderByteRanges = I.renderByteRanges

-- http_renderByteRangesBuilder :: ByteRanges -> Builder
http_renderByteRangesBuilder = I.renderByteRangesBuilder

-- http_methodConnect :: Method
http_methodConnect = I.methodConnect

-- http_methodDelete :: Method
http_methodDelete = I.methodDelete

-- http_methodGet :: Method
http_methodGet = I.methodGet

-- http_methodHead :: Method
http_methodHead = I.methodHead

-- http_methodOptions :: Method
http_methodOptions = I.methodOptions

-- http_methodPatch :: Method
http_methodPatch = I.methodPatch

-- http_methodPost :: Method
http_methodPost = I.methodPost

-- http_methodPut :: Method
http_methodPut = I.methodPut

-- http_methodTrace :: Method
http_methodTrace = I.methodTrace

-- http_parseMethod :: Method -> Either ByteString StdMethod
http_parseMethod = I.parseMethod

-- http_renderMethod :: Either ByteString StdMethod -> Method
http_renderMethod = I.renderMethod

-- http_renderStdMethod :: StdMethod -> Method
http_renderStdMethod = I.renderStdMethod

-- http_accepted202 :: Status
http_accepted202 = I.accepted202

-- http_badGateway502 :: Status
http_badGateway502 = I.badGateway502

-- http_badRequest400 :: Status
http_badRequest400 = I.badRequest400

-- http_conflict409 :: Status
http_conflict409 = I.conflict409

-- http_continue100 :: Status
http_continue100 = I.continue100

-- http_created201 :: Status
http_created201 = I.created201

-- http_expectationFailed417 :: Status
http_expectationFailed417 = I.expectationFailed417

-- http_forbidden403 :: Status
http_forbidden403 = I.forbidden403

-- http_found302 :: Status
http_found302 = I.found302

-- http_gatewayTimeout504 :: Status
http_gatewayTimeout504 = I.gatewayTimeout504

-- http_gone410 :: Status
http_gone410 = I.gone410

-- http_httpVersionNotSupported505 :: Status
http_httpVersionNotSupported505 = I.httpVersionNotSupported505

-- http_imATeaPot418 :: Status
http_imATeaPot418 = I.imATeaPot418

-- http_internalServerError500 :: Status
http_internalServerError500 = I.internalServerError500

-- http_lengthRequired411 :: Status
http_lengthRequired411 = I.lengthRequired411

-- http_methodNotAllowed405 :: Status
http_methodNotAllowed405 = I.methodNotAllowed405

-- http_mkStatus :: Int -> ByteString -> Status
http_mkStatus = I.mkStatus

-- http_movedPermanently301 :: Status
http_movedPermanently301 = I.movedPermanently301

-- http_multipleChoices300 :: Status
http_multipleChoices300 = I.multipleChoices300

-- http_noContent204 :: Status
http_noContent204 = I.noContent204

-- http_nonAuthoritative203 :: Status
http_nonAuthoritative203 = I.nonAuthoritative203

-- http_notAcceptable406 :: Status
http_notAcceptable406 = I.notAcceptable406

-- http_notFound404 :: Status
http_notFound404 = I.notFound404

-- http_notImplemented501 :: Status
http_notImplemented501 = I.notImplemented501

-- http_notModified304 :: Status
http_notModified304 = I.notModified304

-- http_ok200 :: Status
http_ok200 = I.ok200

-- http_partialContent206 :: Status
http_partialContent206 = I.partialContent206

-- http_paymentRequired402 :: Status
http_paymentRequired402 = I.paymentRequired402

-- http_preconditionFailed412 :: Status
http_preconditionFailed412 = I.preconditionFailed412

-- http_proxyAuthenticationRequired407 :: Status
http_proxyAuthenticationRequired407 = I.proxyAuthenticationRequired407

-- http_requestEntityTooLarge413 :: Status
http_requestEntityTooLarge413 = I.requestEntityTooLarge413

-- http_requestTimeout408 :: Status
http_requestTimeout408 = I.requestTimeout408

-- http_requestURITooLong414 :: Status
http_requestURITooLong414 = I.requestURITooLong414

-- http_requestedRangeNotSatisfiable416 :: Status
http_requestedRangeNotSatisfiable416 = I.requestedRangeNotSatisfiable416

-- http_resetContent205 :: Status
http_resetContent205 = I.resetContent205

-- http_seeOther303 :: Status
http_seeOther303 = I.seeOther303

-- http_serviceUnavailable503 :: Status
http_serviceUnavailable503 = I.serviceUnavailable503

-- http_status100 :: Status
http_status100 = I.status100

-- http_status101 :: Status
http_status101 = I.status101

-- http_status200 :: Status
http_status200 = I.status200

-- http_status201 :: Status
http_status201 = I.status201

-- http_status202 :: Status
http_status202 = I.status202

-- http_status203 :: Status
http_status203 = I.status203

-- http_status204 :: Status
http_status204 = I.status204

-- http_status205 :: Status
http_status205 = I.status205

-- http_status206 :: Status
http_status206 = I.status206

-- http_status300 :: Status
http_status300 = I.status300

-- http_status301 :: Status
http_status301 = I.status301

-- http_status302 :: Status
http_status302 = I.status302

-- http_status303 :: Status
http_status303 = I.status303

-- http_status304 :: Status
http_status304 = I.status304

-- http_status305 :: Status
http_status305 = I.status305

-- http_status307 :: Status
http_status307 = I.status307

-- http_status400 :: Status
http_status400 = I.status400

-- http_status401 :: Status
http_status401 = I.status401

-- http_status402 :: Status
http_status402 = I.status402

-- http_status403 :: Status
http_status403 = I.status403

-- http_status404 :: Status
http_status404 = I.status404

-- http_status405 :: Status
http_status405 = I.status405

-- http_status406 :: Status
http_status406 = I.status406

-- http_status407 :: Status
http_status407 = I.status407

-- http_status408 :: Status
http_status408 = I.status408

-- http_status409 :: Status
http_status409 = I.status409

-- http_status410 :: Status
http_status410 = I.status410

-- http_status411 :: Status
http_status411 = I.status411

-- http_status412 :: Status
http_status412 = I.status412

-- http_status413 :: Status
http_status413 = I.status413

-- http_status414 :: Status
http_status414 = I.status414

-- http_status415 :: Status
http_status415 = I.status415

-- http_status416 :: Status
http_status416 = I.status416

-- http_status417 :: Status
http_status417 = I.status417

-- http_status418 :: Status
http_status418 = I.status418

-- http_status500 :: Status
http_status500 = I.status500

-- http_status501 :: Status
http_status501 = I.status501

-- http_status502 :: Status
http_status502 = I.status502

-- http_status503 :: Status
http_status503 = I.status503

-- http_status504 :: Status
http_status504 = I.status504

-- http_status505 :: Status
http_status505 = I.status505

-- http_statusIsClientError :: Status -> Bool
http_statusIsClientError = I.statusIsClientError

-- http_statusIsInformational :: Status -> Bool
http_statusIsInformational = I.statusIsInformational

-- http_statusIsRedirection :: Status -> Bool
http_statusIsRedirection = I.statusIsRedirection

-- http_statusIsServerError :: Status -> Bool
http_statusIsServerError = I.statusIsServerError

-- http_statusIsSuccessful :: Status -> Bool
http_statusIsSuccessful = I.statusIsSuccessful

-- http_switchingProtocols101 :: Status
http_switchingProtocols101 = I.switchingProtocols101

-- http_temporaryRedirect307 :: Status
http_temporaryRedirect307 = I.temporaryRedirect307

-- http_unauthorized401 :: Status
http_unauthorized401 = I.unauthorized401

-- http_unsupportedMediaType415 :: Status
http_unsupportedMediaType415 = I.unsupportedMediaType415

-- http_useProxy305 :: Status
http_useProxy305 = I.useProxy305

-- http_decodePath :: ByteString -> ([Text], Query)
http_decodePath = I.decodePath

-- http_decodePathSegments :: ByteString -> [Text]
http_decodePathSegments = I.decodePathSegments

-- http_encodePath :: [Text] -> Query -> Builder
http_encodePath = I.encodePath

-- http_encodePathSegments :: [Text] -> Builder
http_encodePathSegments = I.encodePathSegments

-- http_encodePathSegmentsRelative :: [Text] -> Builder
http_encodePathSegmentsRelative = I.encodePathSegmentsRelative

-- http_extractPath :: ByteString -> ByteString
http_extractPath = I.extractPath

-- http_parseQuery :: ByteString -> Query
http_parseQuery = I.parseQuery

-- http_parseQueryText :: ByteString -> QueryText
http_parseQueryText = I.parseQueryText

-- http_parseSimpleQuery :: ByteString -> SimpleQuery
http_parseSimpleQuery = I.parseSimpleQuery

-- http_queryTextToQuery :: QueryText -> Query
http_queryTextToQuery = I.queryTextToQuery

-- http_queryToQueryText :: Query -> QueryText
http_queryToQueryText = I.queryToQueryText

-- http_renderQuery :: Bool -> Query -> ByteString
http_renderQuery = I.renderQuery

-- http_renderQueryBuilder :: Bool -> Query -> Builder
http_renderQueryBuilder = I.renderQueryBuilder

-- http_renderQueryText :: Bool -> QueryText -> Builder
http_renderQueryText = I.renderQueryText

-- http_renderSimpleQuery :: Bool -> SimpleQuery -> ByteString
http_renderSimpleQuery = I.renderSimpleQuery

-- http_simpleQueryToQuery :: SimpleQuery -> Query
http_simpleQueryToQuery = I.simpleQueryToQuery

-- http_urlDecode :: Bool -> ByteString -> ByteString
http_urlDecode = I.urlDecode

-- http_urlEncode :: Bool -> ByteString -> ByteString
http_urlEncode = I.urlEncode

-- http_urlEncodeBuilder :: Bool -> ByteString -> Builder
http_urlEncodeBuilder = I.urlEncodeBuilder

-- http_http09 :: HttpVersion
http_http09 = I.http09

-- http_http10 :: HttpVersion
http_http10 = I.http10

-- http_http11 :: HttpVersion
http_http11 = I.http11

type HttpByteRange  = I.ByteRange

-- constructor :: Integer -> ByteRangeFrom
http_mk'ByteRangeFrom =  I.ByteRangeFrom
pattern HttpByteRangeFrom a <-  I.ByteRangeFrom a

-- constructor :: Integer -> Integer -> ByteRangeFromTo
http_mk'ByteRangeFromTo =  I.ByteRangeFromTo
pattern HttpByteRangeFromTo a b <-  I.ByteRangeFromTo a b

-- constructor :: Integer -> ByteRangeSuffix
http_mk'ByteRangeSuffix =  I.ByteRangeSuffix
pattern HttpByteRangeSuffix a <-  I.ByteRangeSuffix a

type HttpByteRanges  = I.ByteRanges

type HttpHeader  = I.Header

type HttpHeaderName  = I.HeaderName

type HttpRequestHeaders  = I.RequestHeaders

type HttpResponseHeaders  = I.ResponseHeaders

type HttpMethod  = I.Method

type HttpStdMethod  = I.StdMethod

-- constructor :: GET
http_mk'GET =  I.GET
pattern HttpGET  <-  I.GET

-- constructor :: POST
http_mk'POST =  I.POST
pattern HttpPOST  <-  I.POST

-- constructor :: HEAD
http_mk'HEAD =  I.HEAD
pattern HttpHEAD  <-  I.HEAD

-- constructor :: PUT
http_mk'PUT =  I.PUT
pattern HttpPUT  <-  I.PUT

-- constructor :: DELETE
http_mk'DELETE =  I.DELETE
pattern HttpDELETE  <-  I.DELETE

-- constructor :: TRACE
http_mk'TRACE =  I.TRACE
pattern HttpTRACE  <-  I.TRACE

-- constructor :: CONNECT
http_mk'CONNECT =  I.CONNECT
pattern HttpCONNECT  <-  I.CONNECT

-- constructor :: OPTIONS
http_mk'OPTIONS =  I.OPTIONS
pattern HttpOPTIONS  <-  I.OPTIONS

-- constructor :: PATCH
http_mk'PATCH =  I.PATCH
pattern HttpPATCH  <-  I.PATCH

type HttpStatus  = I.Status
get_http_statusCode o = I.statusCode o
set_http_statusCode x o = o { I.statusCode = x}
get_http_statusMessage o = I.statusMessage o
set_http_statusMessage x o = o { I.statusMessage = x}

-- constructor :: Int -> ByteString -> Status
http_mk'Status =  I.Status
pattern HttpStatus a b <-  I.Status a b

type HttpQuery  = I.Query

type HttpQueryItem  = I.QueryItem

type HttpQueryText  = I.QueryText

type HttpSimpleQuery  = I.SimpleQuery

type HttpSimpleQueryItem  = I.SimpleQueryItem

type HttpHttpVersion  = I.HttpVersion
get_http_httpMajor o = I.httpMajor o
set_http_httpMajor x o = o { I.httpMajor = x}
get_http_httpMinor o = I.httpMinor o
set_http_httpMinor x o = o { I.httpMinor = x}

-- constructor :: Int -> Int -> HttpVersion
http_mk'HttpVersion =  I.HttpVersion
pattern HttpHttpVersion a b <-  I.HttpVersion a b
