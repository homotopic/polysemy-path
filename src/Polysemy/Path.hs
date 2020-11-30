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
, SomeBase
, PathException
, Path.absdir
, Path.reldir
, Path.absfile
, Path.relfile
, (Path.</>)
, stripProperPrefix
, Path.isProperPrefixOf
, Path.parent
, Path.filename
, Path.dirname
, addExtension
, splitExtension
, fileExtension
, replaceExtension
, parseRelFile
, parseAbsFile
, parseRelDir
, parseAbsDir
, parseSomeDir
, parseSomeFile
, Path.toFilePath
, Path.fromAbsDir
, Path.fromRelDir
, Path.fromAbsFile
, Path.fromRelFile
, Path.fromSomeDir
, Path.fromSomeFile
, Path.mkAbsDir
, Path.mkRelDir
, Path.mkAbsFile
, Path.mkRelFile
) where

import qualified Path
import Path (Path, Rel, Abs, File, Dir, SomeBase, PathException)
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

-- | Polysemy version of `Path.parseSomeDir`.
--
-- @since 0.2.0.0
parseSomeDir :: Members '[Error PathException] r
             => FilePath
             -> Sem r (SomeBase Dir)
parseSomeDir x = irrefutableAbsorbThrow (Path.parseSomeDir x)

-- | Polysemy version of `Path.parseSomeFile`.
--
-- @since 0.2.0.0
parseSomeFile :: Members '[Error PathException] r
              => FilePath
              -> Sem r (SomeBase File)
parseSomeFile x = irrefutableAbsorbThrow (Path.parseSomeFile x)

-- | Polysemy version of `Path.stripProperPrefix`.
--
-- @since 0.1.0.0
stripProperPrefix :: Members '[Error PathException] r
                  => Path b Dir
                  -> Path b t
                  -> Sem r (Path Rel t)
stripProperPrefix x y = irrefutableAbsorbThrow (Path.stripProperPrefix x y)

-- | Polysemy version of `Path.addExtension`.
--
-- @since 0.2.0.0
addExtension :: Members '[Error PathException] r
             => String
             -> Path b File
             -> Sem r (Path b File)
addExtension x y = irrefutableAbsorbThrow (Path.addExtension x y)

-- | Polysemy version of `Path.splitExtension`.
--
-- @since 0.2.0.0
splitExtension :: Members '[Error PathException] r
               => Path b File
               -> Sem r (Path b File, String)
splitExtension x = irrefutableAbsorbThrow (Path.splitExtension x)

-- | Polysemy version of `Path.replaceExtension`.
--
-- @since 0.2.0.0
replaceExtension :: Members '[Error PathException] r
             => String
             -> Path b File
             -> Sem r (Path b File)
replaceExtension x y = irrefutableAbsorbThrow (Path.replaceExtension x y)

-- | Polysemy version of `Path.fileExtension`.
--
-- @since 0.2.0.0
fileExtension :: Members '[Error PathException] r
              => Path b File
              -> Sem r String
fileExtension x = irrefutableAbsorbThrow (Path.fileExtension x)
