module HLS where
import Jetpack

data HLSParams = HLSParams
  { seg_len :: String
  , index_path :: String
  , ts_url :: String
  , ts_path :: String
  }

demoHLSParams = HLSParams
  { seg_len = "String"
  , index_path = "String"
  , ts_url = "String"
  , ts_path = "String"
  }

vlcCmdParam :: HLSParams -> String
vlcCmdParam HLSParams{..} = filter (/= ' ') $ concat
  [ "#std {{"
  , "    access = livehttp {{"
  , "        seglen = {",seg_len,"},"
  , "        delsegs = true,"
  , "        numsegs = 10,"
  , "        index = {",index_path,"},"
  , "        index-url = {",ts_url,"}"
  , "    }},"
  , "    mux = ts{{"
  , "        use-key-frames"
  , "    }},"
  , "    dst = {",ts_path,"}"
  ]
