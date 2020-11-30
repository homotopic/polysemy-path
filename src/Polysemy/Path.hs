{-|
Module      : Polysemy.Path
License     : MIT
Maintainer  : dan.firth@homotopic.tech
Stability   : experimental

Polysemy versions of functions in the path library.
-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Polysemy.Path (
  Path
, Rel
, Abs
, File
, Dir
, PathException
, parseRelFile
, parseAbsFile
, parseRelDir
, parseAbsDir
, stripProperPrefix
) where

import qualified Path
import Path (Path, Rel, Abs, File, Dir, PathException)
import Polysemy
import Polysemy.Error
import Polysemy.Extra

-- | Polysemy version of `Path.parseRelFile`.
--
-- @since 0.1.0.0
parseRelFile :: Members '[Error PathException] r
             => FilePath
             -> Sem r (Path Rel File)
parseRelFile x = irrefutableAbsorbThrow (Path.parseRelFile x)

-- | Polysemy version of `Path.parseAbsFile`.
--
-- @since 0.1.0.0
parseAbsFile :: Members '[Error PathException] r
             => FilePath
             -> Sem r (Path Abs File)
parseAbsFile x = irrefutableAbsorbThrow (Path.parseAbsFile x)

-- | Polysemy version of `Path.parseRelDir`.
--
-- @since 0.1.0.0
parseRelDir :: Members '[Error PathException] r
            => FilePath
            -> Sem r (Path Rel Dir)
parseRelDir x = irrefutableAbsorbThrow (Path.parseRelDir x)

-- | Polysemy version of `Path.parseAbsDir`.
--
-- @since 0.1.0.0
parseAbsDir :: Members '[Error PathException] r
            => FilePath
            -> Sem r (Path Abs Dir)
parseAbsDir x = irrefutableAbsorbThrow (Path.parseAbsDir x)

-- | Polysemy version of `Path.stripProperPrefix`.
--
-- @since 0.1.0.0
stripProperPrefix :: Members '[Error PathException] r
                  => Path b Dir
                  -> Path b t
                  -> Sem r (Path Rel t)
stripProperPrefix x y = irrefutableAbsorbThrow (Path.stripProperPrefix x y)
