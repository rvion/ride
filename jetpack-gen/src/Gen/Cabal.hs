module Gen.Cabal where

import           Data.List
import           Gen.Names

writeCabalFile :: [(String, String)] -> [String] -> IO ()
writeCabalFile reexports deps = writeFile "jetpack/jetpack.cabal" content
  where
    content = concat $
      [ "\nname:                jetpack"
      , "\nversion:             0.2.0.0"
      , "\nsynopsis:            Initial project template from stack"
      , "\ndescription:         Please see README.md"
      , "\nhomepage:            http://github.com/githubuser/jetpack#readme"
      , "\nlicense:             BSD3"
      , "\nauthor:              Rémi Vion"
      , "\nmaintainer:          vion.remi@gmail.com"
      , "\ncopyright:           2016 Author Here"
      , "\ncategory:            Prelude"
      , "\nbuild-type:          Simple"
      , "\ncabal-version:       >=1.10"
      , "\n"
      , "\nlibrary"
      , "\n  hs-source-dirs:      src"
      , "\n  exposed-modules:     JetPack Exports"
      , "\n  other-modules:       "]
      ++ intersperse ", " (map toN reexports) ++
      [ "\n  build-depends:       "] ++ intersperse ", " deps ++
      [ "\n  ghc-options:         -Wall -fno-warn-missing-signatures"
      , "\n  default-extensions:  NoMonomorphismRestriction, FlexibleContexts, MagicHash, DataKinds, RankNTypes, PatternSynonyms, TypeFamilies"
      , "\n  default-language:    Haskell2010"
      , "\n"
      , "\nsource-repository head"
      , "\n  type:     git"
      , "\n  location: https://github.com/rvion/jetpack"
      ]
