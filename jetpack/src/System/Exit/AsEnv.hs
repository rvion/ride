module System.Exit.AsEnv
  ( module System.Exit.AsEnv
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified System.Exit as I


-- env_die :: forall a. String -> IO a
env_die = I.die

-- env_exitFailure :: forall a. IO a
env_exitFailure = I.exitFailure

-- env_exitSuccess :: forall a. IO a
env_exitSuccess = I.exitSuccess

-- env_exitWith :: forall a. ExitCode -> IO a
env_exitWith = I.exitWith

type EnvExitCode  = I.ExitCode

-- constructor :: ExitSuccess
env_mk'ExitSuccess =  I.ExitSuccess
pattern EnvExitSuccess  <-  I.ExitSuccess 

-- constructor :: Int -> ExitFailure
env_mk'ExitFailure =  I.ExitFailure
pattern EnvExitFailure a <-  I.ExitFailure a