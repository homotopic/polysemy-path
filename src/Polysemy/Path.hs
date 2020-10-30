{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Polysemy.Path where

import Control.Monad
import Path
import Polysemy
import Polysemy.ConstraintAbsorber.MonadCatch
import Polysemy.Error


parseRelFile' :: Members '[Error SomeException] r
              => FilePath
              -> Sem r (Path Rel File)
parseRelFile' x = absorbMonadThrow (try . parseRelFile $ x) >>= either throw return

parseAbsFile' :: Members '[Error SomeException] r
              => FilePath
              -> Sem r (Path Abs File)
parseAbsFile' x = absorbMonadThrow (try . parseAbsFile $ x) >>= either throw return

parseRelDir' :: Members '[Error SomeException] r
             => FilePath
             -> Sem r (Path Rel Dir)
parseRelDir' x = absorbMonadThrow (try . parseRelDir $ x) >>= either throw return

parseAbsDir' :: Members '[Error SomeException] r
             => FilePath
             -> Sem r (Path Abs Dir)
parseAbsDir' x = absorbMonadThrow (try . parseAbsDir $ x) >>= either throw return

stripProperPrefix' :: Members '[Error SomeException] r
                   => Path b Dir
                   -> Path b t
                   -> Sem r (Path Rel t)
stripProperPrefix' x y = absorbMonadThrow (try $ stripProperPrefix x y) >>= either throw return
