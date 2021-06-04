{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_graphicstest (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/mcard/.cabal/bin"
libdir     = "/home/mcard/.cabal/lib/x86_64-linux-ghc-8.6.5/graphicstest-0.1.0.0-2EMOv3loETjCfWTRR2B95K-graphicstest"
dynlibdir  = "/home/mcard/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/mcard/.cabal/share/x86_64-linux-ghc-8.6.5/graphicstest-0.1.0.0"
libexecdir = "/home/mcard/.cabal/libexec/x86_64-linux-ghc-8.6.5/graphicstest-0.1.0.0"
sysconfdir = "/home/mcard/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "graphicstest_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "graphicstest_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "graphicstest_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "graphicstest_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "graphicstest_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "graphicstest_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
