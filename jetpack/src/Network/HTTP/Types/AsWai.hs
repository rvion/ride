module Network.HTTP.Types.AsWai
  ( -- unqualified class re-export
  I.QueryLike(I.toQuery)
  , module Network.HTTP.Types.AsWai
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Network.HTTP.Types as I


-- wai_hAccept :: HeaderName
wai_hAccept = I.hAccept

-- wai_hAcceptLanguage :: HeaderName
wai_hAcceptLanguage = I.hAcceptLanguage

-- wai_hAuthorization :: HeaderName
wai_hAuthorization = I.hAuthorization

-- wai_hCacheControl :: HeaderName
wai_hCacheControl = I.hCacheControl

-- wai_hConnection :: HeaderName
wai_hConnection = I.hConnection

-- wai_hContentEncoding :: HeaderName
wai_hContentEncoding = I.hContentEncoding

-- wai_hContentLength :: HeaderName
wai_hContentLength = I.hContentLength

-- wai_hContentMD5 :: HeaderName
wai_hContentMD5 = I.hContentMD5

-- wai_hContentType :: HeaderName
wai_hContentType = I.hContentType

-- wai_hCookie :: HeaderName
wai_hCookie = I.hCookie

-- wai_hDate :: HeaderName
wai_hDate = I.hDate

-- wai_hIfModifiedSince :: HeaderName
wai_hIfModifiedSince = I.hIfModifiedSince

-- wai_hIfRange :: HeaderName
wai_hIfRange = I.hIfRange

-- wai_hLastModified :: HeaderName
wai_hLastModified = I.hLastModified

-- wai_hLocation :: HeaderName
wai_hLocation = I.hLocation

-- wai_hRange :: HeaderName
wai_hRange = I.hRange

-- wai_hReferer :: HeaderName
wai_hReferer = I.hReferer

-- wai_hServer :: HeaderName
wai_hServer = I.hServer

-- wai_hUserAgent :: HeaderName
wai_hUserAgent = I.hUserAgent

-- wai_renderByteRange :: ByteRange -> ByteString
wai_renderByteRange = I.renderByteRange

-- wai_renderByteRangeBuilder :: ByteRange -> Builder
wai_renderByteRangeBuilder = I.renderByteRangeBuilder

-- wai_renderByteRanges :: ByteRanges -> ByteString
wai_renderByteRanges = I.renderByteRanges

-- wai_renderByteRangesBuilder :: ByteRanges -> Builder
wai_renderByteRangesBuilder = I.renderByteRangesBuilder

-- wai_methodConnect :: Method
wai_methodConnect = I.methodConnect

-- wai_methodDelete :: Method
wai_methodDelete = I.methodDelete

-- wai_methodGet :: Method
wai_methodGet = I.methodGet

-- wai_methodHead :: Method
wai_methodHead = I.methodHead

-- wai_methodOptions :: Method
wai_methodOptions = I.methodOptions

-- wai_methodPatch :: Method
wai_methodPatch = I.methodPatch

-- wai_methodPost :: Method
wai_methodPost = I.methodPost

-- wai_methodPut :: Method
wai_methodPut = I.methodPut

-- wai_methodTrace :: Method
wai_methodTrace = I.methodTrace

-- wai_parseMethod :: Method -> Either ByteString StdMethod
wai_parseMethod = I.parseMethod

-- wai_renderMethod :: Either ByteString StdMethod -> Method
wai_renderMethod = I.renderMethod

-- wai_renderStdMethod :: StdMethod -> Method
wai_renderStdMethod = I.renderStdMethod

-- wai_accepted202 :: Status
wai_accepted202 = I.accepted202

-- wai_badGateway502 :: Status
wai_badGateway502 = I.badGateway502

-- wai_badRequest400 :: Status
wai_badRequest400 = I.badRequest400

-- wai_conflict409 :: Status
wai_conflict409 = I.conflict409

-- wai_continue100 :: Status
wai_continue100 = I.continue100

-- wai_created201 :: Status
wai_created201 = I.created201

-- wai_expectationFailed417 :: Status
wai_expectationFailed417 = I.expectationFailed417

-- wai_forbidden403 :: Status
wai_forbidden403 = I.forbidden403

-- wai_found302 :: Status
wai_found302 = I.found302

-- wai_gatewayTimeout504 :: Status
wai_gatewayTimeout504 = I.gatewayTimeout504

-- wai_gone410 :: Status
wai_gone410 = I.gone410

-- wai_httpVersionNotSupported505 :: Status
wai_httpVersionNotSupported505 = I.httpVersionNotSupported505

-- wai_imATeaPot418 :: Status
wai_imATeaPot418 = I.imATeaPot418

-- wai_internalServerError500 :: Status
wai_internalServerError500 = I.internalServerError500

-- wai_lengthRequired411 :: Status
wai_lengthRequired411 = I.lengthRequired411

-- wai_methodNotAllowed405 :: Status
wai_methodNotAllowed405 = I.methodNotAllowed405

-- wai_mkStatus :: Int -> ByteString -> Status
wai_mkStatus = I.mkStatus

-- wai_movedPermanently301 :: Status
wai_movedPermanently301 = I.movedPermanently301

-- wai_multipleChoices300 :: Status
wai_multipleChoices300 = I.multipleChoices300

-- wai_noContent204 :: Status
wai_noContent204 = I.noContent204

-- wai_nonAuthoritative203 :: Status
wai_nonAuthoritative203 = I.nonAuthoritative203

-- wai_notAcceptable406 :: Status
wai_notAcceptable406 = I.notAcceptable406

-- wai_notFound404 :: Status
wai_notFound404 = I.notFound404

-- wai_notImplemented501 :: Status
wai_notImplemented501 = I.notImplemented501

-- wai_notModified304 :: Status
wai_notModified304 = I.notModified304

-- wai_ok200 :: Status
wai_ok200 = I.ok200

-- wai_partialContent206 :: Status
wai_partialContent206 = I.partialContent206

-- wai_paymentRequired402 :: Status
wai_paymentRequired402 = I.paymentRequired402

-- wai_preconditionFailed412 :: Status
wai_preconditionFailed412 = I.preconditionFailed412

-- wai_proxyAuthenticationRequired407 :: Status
wai_proxyAuthenticationRequired407 = I.proxyAuthenticationRequired407

-- wai_requestEntityTooLarge413 :: Status
wai_requestEntityTooLarge413 = I.requestEntityTooLarge413

-- wai_requestTimeout408 :: Status
wai_requestTimeout408 = I.requestTimeout408

-- wai_requestURITooLong414 :: Status
wai_requestURITooLong414 = I.requestURITooLong414

-- wai_requestedRangeNotSatisfiable416 :: Status
wai_requestedRangeNotSatisfiable416 = I.requestedRangeNotSatisfiable416

-- wai_resetContent205 :: Status
wai_resetContent205 = I.resetContent205

-- wai_seeOther303 :: Status
wai_seeOther303 = I.seeOther303

-- wai_serviceUnavailable503 :: Status
wai_serviceUnavailable503 = I.serviceUnavailable503

-- wai_status100 :: Status
wai_status100 = I.status100

-- wai_status101 :: Status
wai_status101 = I.status101

-- wai_status200 :: Status
wai_status200 = I.status200

-- wai_status201 :: Status
wai_status201 = I.status201

-- wai_status202 :: Status
wai_status202 = I.status202

-- wai_status203 :: Status
wai_status203 = I.status203

-- wai_status204 :: Status
wai_status204 = I.status204

-- wai_status205 :: Status
wai_status205 = I.status205

-- wai_status206 :: Status
wai_status206 = I.status206

-- wai_status300 :: Status
wai_status300 = I.status300

-- wai_status301 :: Status
wai_status301 = I.status301

-- wai_status302 :: Status
wai_status302 = I.status302

-- wai_status303 :: Status
wai_status303 = I.status303

-- wai_status304 :: Status
wai_status304 = I.status304

-- wai_status305 :: Status
wai_status305 = I.status305

-- wai_status307 :: Status
wai_status307 = I.status307

-- wai_status400 :: Status
wai_status400 = I.status400

-- wai_status401 :: Status
wai_status401 = I.status401

-- wai_status402 :: Status
wai_status402 = I.status402

-- wai_status403 :: Status
wai_status403 = I.status403

-- wai_status404 :: Status
wai_status404 = I.status404

-- wai_status405 :: Status
wai_status405 = I.status405

-- wai_status406 :: Status
wai_status406 = I.status406

-- wai_status407 :: Status
wai_status407 = I.status407

-- wai_status408 :: Status
wai_status408 = I.status408

-- wai_status409 :: Status
wai_status409 = I.status409

-- wai_status410 :: Status
wai_status410 = I.status410

-- wai_status411 :: Status
wai_status411 = I.status411

-- wai_status412 :: Status
wai_status412 = I.status412

-- wai_status413 :: Status
wai_status413 = I.status413

-- wai_status414 :: Status
wai_status414 = I.status414

-- wai_status415 :: Status
wai_status415 = I.status415

-- wai_status416 :: Status
wai_status416 = I.status416

-- wai_status417 :: Status
wai_status417 = I.status417

-- wai_status418 :: Status
wai_status418 = I.status418

-- wai_status500 :: Status
wai_status500 = I.status500

-- wai_status501 :: Status
wai_status501 = I.status501

-- wai_status502 :: Status
wai_status502 = I.status502

-- wai_status503 :: Status
wai_status503 = I.status503

-- wai_status504 :: Status
wai_status504 = I.status504

-- wai_status505 :: Status
wai_status505 = I.status505

-- wai_statusIsClientError :: Status -> Bool
wai_statusIsClientError = I.statusIsClientError

-- wai_statusIsInformational :: Status -> Bool
wai_statusIsInformational = I.statusIsInformational

-- wai_statusIsRedirection :: Status -> Bool
wai_statusIsRedirection = I.statusIsRedirection

-- wai_statusIsServerError :: Status -> Bool
wai_statusIsServerError = I.statusIsServerError

-- wai_statusIsSuccessful :: Status -> Bool
wai_statusIsSuccessful = I.statusIsSuccessful

-- wai_switchingProtocols101 :: Status
wai_switchingProtocols101 = I.switchingProtocols101

-- wai_temporaryRedirect307 :: Status
wai_temporaryRedirect307 = I.temporaryRedirect307

-- wai_unauthorized401 :: Status
wai_unauthorized401 = I.unauthorized401

-- wai_unsupportedMediaType415 :: Status
wai_unsupportedMediaType415 = I.unsupportedMediaType415

-- wai_useProxy305 :: Status
wai_useProxy305 = I.useProxy305

-- wai_decodePath :: ByteString -> ([Text], Query)
wai_decodePath = I.decodePath

-- wai_decodePathSegments :: ByteString -> [Text]
wai_decodePathSegments = I.decodePathSegments

-- wai_encodePath :: [Text] -> Query -> Builder
wai_encodePath = I.encodePath

-- wai_encodePathSegments :: [Text] -> Builder
wai_encodePathSegments = I.encodePathSegments

-- wai_encodePathSegmentsRelative :: [Text] -> Builder
wai_encodePathSegmentsRelative = I.encodePathSegmentsRelative

-- wai_extractPath :: ByteString -> ByteString
wai_extractPath = I.extractPath

-- wai_parseQuery :: ByteString -> Query
wai_parseQuery = I.parseQuery

-- wai_parseQueryText :: ByteString -> QueryText
wai_parseQueryText = I.parseQueryText

-- wai_parseSimpleQuery :: ByteString -> SimpleQuery
wai_parseSimpleQuery = I.parseSimpleQuery

-- wai_queryTextToQuery :: QueryText -> Query
wai_queryTextToQuery = I.queryTextToQuery

-- wai_queryToQueryText :: Query -> QueryText
wai_queryToQueryText = I.queryToQueryText

-- wai_renderQuery :: Bool -> Query -> ByteString
wai_renderQuery = I.renderQuery

-- wai_renderQueryBuilder :: Bool -> Query -> Builder
wai_renderQueryBuilder = I.renderQueryBuilder

-- wai_renderQueryText :: Bool -> QueryText -> Builder
wai_renderQueryText = I.renderQueryText

-- wai_renderSimpleQuery :: Bool -> SimpleQuery -> ByteString
wai_renderSimpleQuery = I.renderSimpleQuery

-- wai_simpleQueryToQuery :: SimpleQuery -> Query
wai_simpleQueryToQuery = I.simpleQueryToQuery

-- wai_urlDecode :: Bool -> ByteString -> ByteString
wai_urlDecode = I.urlDecode

-- wai_urlEncode :: Bool -> ByteString -> ByteString
wai_urlEncode = I.urlEncode

-- wai_urlEncodeBuilder :: Bool -> ByteString -> Builder
wai_urlEncodeBuilder = I.urlEncodeBuilder

-- wai_http09 :: HttpVersion
wai_http09 = I.http09

-- wai_http10 :: HttpVersion
wai_http10 = I.http10

-- wai_http11 :: HttpVersion
wai_http11 = I.http11

type WaiByteRange  = I.ByteRange

-- constructor :: Integer -> ByteRangeFrom
wai_mk'ByteRangeFrom =  I.ByteRangeFrom
pattern WaiByteRangeFrom a <-  I.ByteRangeFrom a

-- constructor :: Integer -> Integer -> ByteRangeFromTo
wai_mk'ByteRangeFromTo =  I.ByteRangeFromTo
pattern WaiByteRangeFromTo a b <-  I.ByteRangeFromTo a b

-- constructor :: Integer -> ByteRangeSuffix
wai_mk'ByteRangeSuffix =  I.ByteRangeSuffix
pattern WaiByteRangeSuffix a <-  I.ByteRangeSuffix a

type WaiByteRanges  = I.ByteRanges

type WaiHeader  = I.Header

type WaiHeaderName  = I.HeaderName

type WaiRequestHeaders  = I.RequestHeaders

type WaiResponseHeaders  = I.ResponseHeaders

type WaiMethod  = I.Method

type WaiStdMethod  = I.StdMethod

-- constructor :: GET
wai_mk'GET =  I.GET
pattern WaiGET  <-  I.GET

-- constructor :: POST
wai_mk'POST =  I.POST
pattern WaiPOST  <-  I.POST

-- constructor :: HEAD
wai_mk'HEAD =  I.HEAD
pattern WaiHEAD  <-  I.HEAD

-- constructor :: PUT
wai_mk'PUT =  I.PUT
pattern WaiPUT  <-  I.PUT

-- constructor :: DELETE
wai_mk'DELETE =  I.DELETE
pattern WaiDELETE  <-  I.DELETE

-- constructor :: TRACE
wai_mk'TRACE =  I.TRACE
pattern WaiTRACE  <-  I.TRACE

-- constructor :: CONNECT
wai_mk'CONNECT =  I.CONNECT
pattern WaiCONNECT  <-  I.CONNECT

-- constructor :: OPTIONS
wai_mk'OPTIONS =  I.OPTIONS
pattern WaiOPTIONS  <-  I.OPTIONS

-- constructor :: PATCH
wai_mk'PATCH =  I.PATCH
pattern WaiPATCH  <-  I.PATCH

type WaiStatus  = I.Status
get_wai_statusCode o = I.statusCode o
set_wai_statusCode x o = o { I.statusCode = x}
get_wai_statusMessage o = I.statusMessage o
set_wai_statusMessage x o = o { I.statusMessage = x}

-- constructor :: Int -> ByteString -> Status
wai_mk'Status =  I.Status
pattern WaiStatus a b <-  I.Status a b

type WaiQuery  = I.Query

type WaiQueryItem  = I.QueryItem

type WaiQueryText  = I.QueryText

type WaiSimpleQuery  = I.SimpleQuery

type WaiSimpleQueryItem  = I.SimpleQueryItem

type WaiHttpVersion  = I.HttpVersion
get_wai_httpMajor o = I.httpMajor o
set_wai_httpMajor x o = o { I.httpMajor = x}
get_wai_httpMinor o = I.httpMinor o
set_wai_httpMinor x o = o { I.httpMinor = x}

-- constructor :: Int -> Int -> HttpVersion
wai_mk'HttpVersion =  I.HttpVersion
pattern WaiHttpVersion a b <-  I.HttpVersion a b