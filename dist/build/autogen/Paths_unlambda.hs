module Paths_unlambda (
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
version = Version [0,1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/sidharta/.cabal/bin"
libdir     = "/Users/sidharta/.cabal/lib/x86_64-osx-ghc-7.8.4/unlambda-0.1.0"
datadir    = "/Users/sidharta/.cabal/share/x86_64-osx-ghc-7.8.4/unlambda-0.1.0"
libexecdir = "/Users/sidharta/.cabal/libexec"
sysconfdir = "/Users/sidharta/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "unlambda_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "unlambda_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "unlambda_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "unlambda_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "unlambda_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
