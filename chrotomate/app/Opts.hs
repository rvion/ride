module Opts where
import           Options.Applicative

data Opts = Opts { db :: FilePath }

opts :: Parser Opts
opts = Opts <$> strOption
  ( long "db"
    <> help "file to use as DB"
    <> value defaultDB
    )


defaultDB = "/Users/RemiVion/dev/chrotomate/testdb"