module Network.Wai.Middleware.RequestLogger.AsWai
  ( module Network.Wai.Middleware.RequestLogger.AsWai
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Network.Wai.Middleware.RequestLogger as I


-- wai_logStdout :: Middleware
wai_logStdout = I.logStdout

-- wai_logStdoutDev :: Middleware
wai_logStdoutDev = I.logStdoutDev

-- wai_mkRequestLogger :: RequestLoggerSettings -> IO Middleware
wai_mkRequestLogger = I.mkRequestLogger

type WaiCallback  = I.Callback

type WaiDestination  = I.Destination

-- constructor :: Handle -> Handle
wai_mk'Handle =  I.Handle
pattern WaiHandle a <-  I.Handle a

-- constructor :: LoggerSet -> Logger
wai_mk'Logger =  I.Logger
pattern WaiLogger a <-  I.Logger a

-- constructor :: Callback -> Callback
wai_mk'Callback =  I.Callback
pattern WaiCallback a <-  I.Callback a

type WaiOutputFormat  = I.OutputFormat

-- constructor :: IPAddrSource -> Apache
wai_mk'Apache =  I.Apache
pattern WaiApache a <-  I.Apache a

-- constructor :: Bool -> Detailed
wai_mk'Detailed =  I.Detailed
pattern WaiDetailed a <-  I.Detailed a

-- constructor :: OutputFormatter -> CustomOutputFormat
wai_mk'CustomOutputFormat =  I.CustomOutputFormat
pattern WaiCustomOutputFormat a <-  I.CustomOutputFormat a

-- constructor :: OutputFormatterWithDetails -> CustomOutputFormatWithDetails
wai_mk'CustomOutputFormatWithDetails =  I.CustomOutputFormatWithDetails
pattern WaiCustomOutputFormatWithDetails a <-  I.CustomOutputFormatWithDetails a

type WaiOutputFormatter  = I.OutputFormatter

type WaiOutputFormatterWithDetails  = I.OutputFormatterWithDetails

type WaiRequestLoggerSettings  = I.RequestLoggerSettings
get_wai_outputFormat o = I.outputFormat o
set_wai_outputFormat x o = o { I.outputFormat = x}
get_wai_autoFlush o = I.autoFlush o
set_wai_autoFlush x o = o { I.autoFlush = x}
get_wai_destination o = I.destination o
set_wai_destination x o = o { I.destination = x}

type WaiIPAddrSource  = I.IPAddrSource

-- constructor :: FromSocket
wai_mk'FromSocket =  I.FromSocket
pattern WaiFromSocket  <-  I.FromSocket

-- constructor :: FromHeader
wai_mk'FromHeader =  I.FromHeader
pattern WaiFromHeader  <-  I.FromHeader

-- constructor :: FromFallback
wai_mk'FromFallback =  I.FromFallback
pattern WaiFromFallback  <-  I.FromFallback
