{-# LANGUAGE TemplateHaskell #-}
module Distribution.CabalHoogle.HoogleConfig
  ( -- data
    HoogleConfig(..)
    -- defaults
  , mkDefaultConfig
  , defaultMinHoogleVersion
  , defaultHooglePackageName
  , defaultMinHoogleId
    -- optics
  , hooglePackageName
  , minHoogleVersion
  , minHoogleId
  , hoogleProjectRoot
  , hoogleDatabasePath
  ) where


import System.Directory (getCurrentDirectory, makeRelativeToCurrentDirectory)

import Control.Lens (makeLenses)

import Distribution.Types.PackageName (PackageName, mkPackageName)
import Distribution.Types.PackageId (PackageIdentifier(..))
import Distribution.Version (Version, mkVersion)


-- | Data ----------------------------------------

-- | 'HoogleConfig' should give the minimal set of
-- information necessary to work with Hoogle, including:
--
--  * Hoogle package name for installation
--  * Hoogle package identifier for installation
--  * Minimum Hoogle version supported (default 5.0)
--
data HoogleConfig = HoogleConfig
  { _hooglePackageName  :: PackageName
  , _minHoogleVersion   :: Version
  , _minHoogleId        :: PackageIdentifier
  , _hoogleProjectRoot  :: FilePath
  , _hoogleDatabasePath :: FilePath
  }
makeLenses ''HoogleConfig


mkDefaultConfig :: IO HoogleConfig
mkDefaultConfig = do
  hoogleRoot   <- defaultHoogleRoot
  hoogleDbPath <- defaultHoogleDatabasePath
  pure $ HoogleConfig
    defaultHooglePackageName
    defaultMinHoogleVersion
    defaultMinHoogleId
    hoogleRoot
    hoogleDbPath

defaultMinHoogleId :: PackageIdentifier
defaultMinHoogleId = PackageIdentifier
  defaultHooglePackageName
  defaultMinHoogleVersion

defaultMinHoogleVersion :: Version
defaultMinHoogleVersion = mkVersion [5, 0]

defaultHooglePackageName :: PackageName
defaultHooglePackageName = mkPackageName "hoogle"

defaultHoogleRoot :: IO FilePath
defaultHoogleRoot = do
  cwd  <- getCurrentDirectory
  hDir <- makeRelativeToCurrentDirectory ".hoogle"
  pure $ cwd <> "/" <> hDir

defaultHoogleDatabasePath :: IO FilePath
defaultHoogleDatabasePath = do
  root <- defaultHoogleRoot
  pure $ root <> "/" <> "database.hoo"
