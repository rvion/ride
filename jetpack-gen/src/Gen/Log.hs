module Gen.Log where
import           System.Console.ANSI

asStep, asSuccess, asInfo, asWarning, asError :: IO a -> IO a
asStep = writeIn Blue
asSuccess = writeIn Green
asInfo = writeIn Cyan
asWarning = writeIn Yellow
asError = writeIn Red

writeIn :: Color -> IO a -> IO a
writeIn col x = do
  setSGR [SetColor Foreground Vivid col]
  _r <- x
  setSGR [Reset]
  return _r

