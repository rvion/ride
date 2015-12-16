module System.IO.AsEnv
  ( module System.IO.AsEnv
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified System.IO as I


-- env_char8 :: TextEncoding
env_char8 = I.char8

-- env_latin1 :: TextEncoding
env_latin1 = I.latin1

-- env_mkTextEncoding :: String -> IO TextEncoding
env_mkTextEncoding = I.mkTextEncoding

-- env_utf16 :: TextEncoding
env_utf16 = I.utf16

-- env_utf16be :: TextEncoding
env_utf16be = I.utf16be

-- env_utf16le :: TextEncoding
env_utf16le = I.utf16le

-- env_utf32 :: TextEncoding
env_utf32 = I.utf32

-- env_utf32be :: TextEncoding
env_utf32be = I.utf32be

-- env_utf32le :: TextEncoding
env_utf32le = I.utf32le

-- env_utf8 :: TextEncoding
env_utf8 = I.utf8

-- env_utf8_bom :: TextEncoding
env_utf8_bom = I.utf8_bom

-- env_hClose :: Handle -> IO ()
env_hClose = I.hClose

-- env_hFileSize :: Handle -> IO Integer
env_hFileSize = I.hFileSize

-- env_hFlush :: Handle -> IO ()
env_hFlush = I.hFlush

-- env_hGetBuffering :: Handle -> IO BufferMode
env_hGetBuffering = I.hGetBuffering

-- env_hGetEcho :: Handle -> IO Bool
env_hGetEcho = I.hGetEcho

-- env_hGetEncoding :: Handle -> IO (Maybe TextEncoding)
env_hGetEncoding = I.hGetEncoding

-- env_hGetPosn :: Handle -> IO HandlePosn
env_hGetPosn = I.hGetPosn

-- env_hIsClosed :: Handle -> IO Bool
env_hIsClosed = I.hIsClosed

-- env_hIsEOF :: Handle -> IO Bool
env_hIsEOF = I.hIsEOF

-- env_hIsOpen :: Handle -> IO Bool
env_hIsOpen = I.hIsOpen

-- env_hIsReadable :: Handle -> IO Bool
env_hIsReadable = I.hIsReadable

-- env_hIsSeekable :: Handle -> IO Bool
env_hIsSeekable = I.hIsSeekable

-- env_hIsTerminalDevice :: Handle -> IO Bool
env_hIsTerminalDevice = I.hIsTerminalDevice

-- env_hIsWritable :: Handle -> IO Bool
env_hIsWritable = I.hIsWritable

-- env_hLookAhead :: Handle -> IO Char
env_hLookAhead = I.hLookAhead

-- env_hSeek :: Handle -> SeekMode -> Integer -> IO ()
env_hSeek = I.hSeek

-- env_hSetBinaryMode :: Handle -> Bool -> IO ()
env_hSetBinaryMode = I.hSetBinaryMode

-- env_hSetBuffering :: Handle -> BufferMode -> IO ()
env_hSetBuffering = I.hSetBuffering

-- env_hSetEcho :: Handle -> Bool -> IO ()
env_hSetEcho = I.hSetEcho

-- env_hSetEncoding :: Handle -> TextEncoding -> IO ()
env_hSetEncoding = I.hSetEncoding

-- env_hSetFileSize :: Handle -> Integer -> IO ()
env_hSetFileSize = I.hSetFileSize

-- env_hSetNewlineMode :: Handle -> NewlineMode -> IO ()
env_hSetNewlineMode = I.hSetNewlineMode

-- env_hSetPosn :: HandlePosn -> IO ()
env_hSetPosn = I.hSetPosn

-- env_hShow :: Handle -> IO String
env_hShow = I.hShow

-- env_hTell :: Handle -> IO Integer
env_hTell = I.hTell

-- env_isEOF :: IO Bool
env_isEOF = I.isEOF

-- env_openBinaryFile :: FilePath -> IOMode -> IO Handle
env_openBinaryFile = I.openBinaryFile

-- env_openFile :: FilePath -> IOMode -> IO Handle
env_openFile = I.openFile

-- env_stderr :: Handle
env_stderr = I.stderr

-- env_stdin :: Handle
env_stdin = I.stdin

-- env_stdout :: Handle
env_stdout = I.stdout

-- env_hGetBuf :: forall a. Handle -> Ptr a -> Int -> IO Int
env_hGetBuf = I.hGetBuf

-- env_hGetBufNonBlocking :: forall a. Handle -> Ptr a -> Int -> IO Int
env_hGetBufNonBlocking = I.hGetBufNonBlocking

-- env_hGetBufSome :: forall a. Handle -> Ptr a -> Int -> IO Int
env_hGetBufSome = I.hGetBufSome

-- env_hGetChar :: Handle -> IO Char
env_hGetChar = I.hGetChar

-- env_hGetContents :: Handle -> IO String
env_hGetContents = I.hGetContents

-- env_hGetLine :: Handle -> IO String
env_hGetLine = I.hGetLine

-- env_hPutBuf :: forall a. Handle -> Ptr a -> Int -> IO ()
env_hPutBuf = I.hPutBuf

-- env_hPutBufNonBlocking :: forall a. Handle -> Ptr a -> Int -> IO Int
env_hPutBufNonBlocking = I.hPutBufNonBlocking

-- env_hPutChar :: Handle -> Char -> IO ()
env_hPutChar = I.hPutChar

-- env_hPutStr :: Handle -> String -> IO ()
env_hPutStr = I.hPutStr

-- env_hPutStrLn :: Handle -> String -> IO ()
env_hPutStrLn = I.hPutStrLn

-- env_hWaitForInput :: Handle -> Int -> IO Bool
env_hWaitForInput = I.hWaitForInput

-- env_nativeNewline :: Newline
env_nativeNewline = I.nativeNewline

-- env_nativeNewlineMode :: NewlineMode
env_nativeNewlineMode = I.nativeNewlineMode

-- env_noNewlineTranslation :: NewlineMode
env_noNewlineTranslation = I.noNewlineTranslation

-- env_universalNewlineMode :: NewlineMode
env_universalNewlineMode = I.universalNewlineMode

-- env_appendFile :: FilePath -> String -> IO ()
env_appendFile = I.appendFile

-- env_fixIO :: forall a. (a -> IO a) -> IO a
env_fixIO = I.fixIO

-- env_getChar :: IO Char
env_getChar = I.getChar

-- env_getContents :: IO String
env_getContents = I.getContents

-- env_getLine :: IO String
env_getLine = I.getLine

-- env_hPrint :: forall a. Show a => Handle -> a -> IO ()
env_hPrint = I.hPrint

-- env_hReady :: Handle -> IO Bool
env_hReady = I.hReady

-- env_interact :: (String -> String) -> IO ()
env_interact = I.interact

-- env_localeEncoding :: TextEncoding
env_localeEncoding = I.localeEncoding

-- env_openBinaryTempFile :: FilePath -> String -> IO (FilePath, Handle)
env_openBinaryTempFile = I.openBinaryTempFile

-- env_openBinaryTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)
env_openBinaryTempFileWithDefaultPermissions = I.openBinaryTempFileWithDefaultPermissions

-- env_openTempFile :: FilePath -> String -> IO (FilePath, Handle)
env_openTempFile = I.openTempFile

-- env_openTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)
env_openTempFileWithDefaultPermissions = I.openTempFileWithDefaultPermissions

-- env_print :: forall a. Show a => a -> IO ()
env_print = I.print

-- env_putChar :: Char -> IO ()
env_putChar = I.putChar

-- env_putStr :: String -> IO ()
env_putStr = I.putStr

-- env_putStrLn :: String -> IO ()
env_putStrLn = I.putStrLn

-- env_readFile :: FilePath -> IO String
env_readFile = I.readFile

-- env_readIO :: forall a. Read a => String -> IO a
env_readIO = I.readIO

-- env_readLn :: forall a. Read a => IO a
env_readLn = I.readLn

-- env_withBinaryFile :: forall r. FilePath -> IOMode -> (Handle -> IO r) -> IO r
env_withBinaryFile = I.withBinaryFile

-- env_withFile :: forall r. FilePath -> IOMode -> (Handle -> IO r) -> IO r
env_withFile = I.withFile

-- env_writeFile :: FilePath -> String -> IO ()
env_writeFile = I.writeFile

type EnvFilePath  = I.FilePath

type EnvSeekMode  = I.SeekMode

-- constructor :: AbsoluteSeek
env_mk'AbsoluteSeek =  I.AbsoluteSeek
pattern EnvAbsoluteSeek  <-  I.AbsoluteSeek 

-- constructor :: RelativeSeek
env_mk'RelativeSeek =  I.RelativeSeek
pattern EnvRelativeSeek  <-  I.RelativeSeek 

-- constructor :: SeekFromEnd
env_mk'SeekFromEnd =  I.SeekFromEnd
pattern EnvSeekFromEnd  <-  I.SeekFromEnd 

type EnvTextEncoding  = I.TextEncoding

type EnvHandlePosn  = I.HandlePosn

type EnvBufferMode  = I.BufferMode

-- constructor :: NoBuffering
env_mk'NoBuffering =  I.NoBuffering
pattern EnvNoBuffering  <-  I.NoBuffering 

-- constructor :: LineBuffering
env_mk'LineBuffering =  I.LineBuffering
pattern EnvLineBuffering  <-  I.LineBuffering 

-- constructor :: Maybe Int -> BlockBuffering
env_mk'BlockBuffering =  I.BlockBuffering
pattern EnvBlockBuffering a <-  I.BlockBuffering a

type EnvHandle  = I.Handle

type EnvNewline  = I.Newline

-- constructor :: LF
env_mk'LF =  I.LF
pattern EnvLF  <-  I.LF 

-- constructor :: CRLF
env_mk'CRLF =  I.CRLF
pattern EnvCRLF  <-  I.CRLF 

type EnvNewlineMode  = I.NewlineMode
get_env_inputNL o = I.inputNL o
set_env_inputNL x o = o { I.inputNL = x}
get_env_outputNL o = I.outputNL o
set_env_outputNL x o = o { I.outputNL = x}

-- constructor :: Newline -> Newline -> NewlineMode
env_mk'NewlineMode =  I.NewlineMode
pattern EnvNewlineMode a b <-  I.NewlineMode a b

type EnvIOMode  = I.IOMode

-- constructor :: ReadMode
env_mk'ReadMode =  I.ReadMode
pattern EnvReadMode  <-  I.ReadMode 

-- constructor :: WriteMode
env_mk'WriteMode =  I.WriteMode
pattern EnvWriteMode  <-  I.WriteMode 

-- constructor :: AppendMode
env_mk'AppendMode =  I.AppendMode
pattern EnvAppendMode  <-  I.AppendMode 

-- constructor :: ReadWriteMode
env_mk'ReadWriteMode =  I.ReadWriteMode
pattern EnvReadWriteMode  <-  I.ReadWriteMode 

type EnvIO a = I.IO a