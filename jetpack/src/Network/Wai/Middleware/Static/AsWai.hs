module Network.Wai.Middleware.Static.AsWai
  ( module Network.Wai.Middleware.Static.AsWai
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Network.Wai.Middleware.Static as I


-- (>->) :: Policy -> Policy -> Policy
(>->) = (I.>->)

-- wai_addBase :: String -> Policy
wai_addBase = I.addBase

-- wai_addSlash :: Policy
wai_addSlash = I.addSlash

-- wai_contains :: String -> Policy
wai_contains = I.contains

-- wai_getMimeType :: FilePath -> MimeType
wai_getMimeType = I.getMimeType

-- wai_hasPrefix :: String -> Policy
wai_hasPrefix = I.hasPrefix

-- wai_hasSuffix :: String -> Policy
wai_hasSuffix = I.hasSuffix

-- wai_initCaching :: CachingStrategy -> IO CacheContainer
wai_initCaching = I.initCaching

-- wai_isNotAbsolute :: Policy
wai_isNotAbsolute = I.isNotAbsolute

-- wai_noDots :: Policy
wai_noDots = I.noDots

-- wai_only :: [(String, String)] -> Policy
wai_only = I.only

-- wai_policy :: (String -> Maybe String) -> Policy
wai_policy = I.policy

-- wai_predicate :: (String -> Bool) -> Policy
wai_predicate = I.predicate

-- wai_static :: Middleware
wai_static = I.static

-- wai_static' :: CacheContainer -> Middleware
wai_static' = I.static'

-- wai_staticPolicy :: Policy -> Middleware
wai_staticPolicy = I.staticPolicy

-- wai_staticPolicy' :: CacheContainer -> Policy -> Middleware
wai_staticPolicy' = I.staticPolicy'

-- wai_unsafeStaticPolicy :: Policy -> Middleware
wai_unsafeStaticPolicy = I.unsafeStaticPolicy

-- wai_unsafeStaticPolicy' :: CacheContainer -> Policy -> Middleware
wai_unsafeStaticPolicy' = I.unsafeStaticPolicy'

type WaiCacheContainer  = I.CacheContainer

type WaiCachingStrategy  = I.CachingStrategy

-- constructor :: NoCaching
wai_mk'NoCaching =  I.NoCaching
pattern WaiNoCaching  <-  I.NoCaching 

-- constructor :: PublicStaticCaching
wai_mk'PublicStaticCaching =  I.PublicStaticCaching
pattern WaiPublicStaticCaching  <-  I.PublicStaticCaching 

-- constructor :: FileMeta -> RequestHeaders -> CustomCaching
wai_mk'CustomCaching =  I.CustomCaching
pattern WaiCustomCaching a <-  I.CustomCaching a

type WaiFileMeta  = I.FileMeta

-- constructor :: ByteString -> ByteString -> FilePath -> FileMeta
wai_mk'FileMeta =  I.FileMeta
pattern WaiFileMeta a b c <-  I.FileMeta a b c

type WaiPolicy  = I.Policy
