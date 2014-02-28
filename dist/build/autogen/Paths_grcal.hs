module Paths_grcal (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/grantmarshall/.cabal/bin"
libdir     = "/Users/grantmarshall/.cabal/lib/x86_64-osx-ghc-7.6.3/grcal-0.1.0.0"
datadir    = "/Users/grantmarshall/.cabal/share/x86_64-osx-ghc-7.6.3/grcal-0.1.0.0"
libexecdir = "/Users/grantmarshall/.cabal/libexec"
sysconfdir = "/Users/grantmarshall/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "grcal_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "grcal_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "grcal_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "grcal_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "grcal_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
