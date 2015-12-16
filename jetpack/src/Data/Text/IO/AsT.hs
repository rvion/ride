module Data.Text.IO.AsT
  ( module Data.Text.IO.AsT
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Data.Text.IO as I


-- t_appendFile :: FilePath -> Text -> IO ()
t_appendFile = I.appendFile

-- t_getContents :: IO Text
t_getContents = I.getContents

-- t_getLine :: IO Text
t_getLine = I.getLine

-- t_hGetChunk :: Handle -> IO Text
t_hGetChunk = I.hGetChunk

-- t_hGetContents :: Handle -> IO Text
t_hGetContents = I.hGetContents

-- t_hGetLine :: Handle -> IO Text
t_hGetLine = I.hGetLine

-- t_hPutStr :: Handle -> Text -> IO ()
t_hPutStr = I.hPutStr

-- t_hPutStrLn :: Handle -> Text -> IO ()
t_hPutStrLn = I.hPutStrLn

-- t_interact :: (Text -> Text) -> IO ()
t_interact = I.interact

-- t_putStr :: Text -> IO ()
t_putStr = I.putStr

-- t_putStrLn :: Text -> IO ()
t_putStrLn = I.putStrLn

-- t_readFile :: FilePath -> IO Text
t_readFile = I.readFile

-- t_writeFile :: FilePath -> Text -> IO ()
t_writeFile = I.writeFile
