module Data.Text.Lazy.IO.AsLt where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Data.Text.Lazy.IO as I

-- lt_appendFile :: FilePath -> Text -> IO ()
lt_appendFile = I.appendFile

-- lt_getContents :: IO Text
lt_getContents = I.getContents

-- lt_getLine :: IO Text
lt_getLine = I.getLine

-- lt_hGetContents :: Handle -> IO Text
lt_hGetContents = I.hGetContents

-- lt_hGetLine :: Handle -> IO Text
lt_hGetLine = I.hGetLine

-- lt_hPutStr :: Handle -> Text -> IO ()
lt_hPutStr = I.hPutStr

-- lt_hPutStrLn :: Handle -> Text -> IO ()
lt_hPutStrLn = I.hPutStrLn

-- lt_interact :: (Text -> Text) -> IO ()
lt_interact = I.interact

-- lt_putStr :: Text -> IO ()
lt_putStr = I.putStr

-- lt_putStrLn :: Text -> IO ()
lt_putStrLn = I.putStrLn

-- lt_readFile :: FilePath -> IO Text
lt_readFile = I.readFile

-- lt_writeFile :: FilePath -> Text -> IO ()
lt_writeFile = I.writeFile

