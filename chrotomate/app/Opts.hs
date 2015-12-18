module Opts where
-- import           Options.Applicative
import           JetPack
data Opts = Opts { db :: FilePath }

opts :: OptParser Opts
opts = Opts <$> opt_strOption
    ( opt_long "db"
   <> opt_help "file to use as DB"
   <> opt_value defaultDB
    )


defaultDB = "/Users/RemiVion/dev/chrotomate/testdb"
