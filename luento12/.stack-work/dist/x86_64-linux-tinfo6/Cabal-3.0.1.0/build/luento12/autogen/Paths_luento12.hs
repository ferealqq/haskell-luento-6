{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_luento12 (
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

bindir     = "/home/peke/Documents/Koulu/FunktioOhjelmointi/luento12/.stack-work/install/x86_64-linux-tinfo6/f7b920acfb8162e6bc88428bb845ddcb5978cb961543c0b65d50a83a591b6f60/8.8.4/bin"
libdir     = "/home/peke/Documents/Koulu/FunktioOhjelmointi/luento12/.stack-work/install/x86_64-linux-tinfo6/f7b920acfb8162e6bc88428bb845ddcb5978cb961543c0b65d50a83a591b6f60/8.8.4/lib/x86_64-linux-ghc-8.8.4/luento12-0.1.0.0-H4sQTi1gvfe36ISCdpXWoi-luento12"
dynlibdir  = "/home/peke/Documents/Koulu/FunktioOhjelmointi/luento12/.stack-work/install/x86_64-linux-tinfo6/f7b920acfb8162e6bc88428bb845ddcb5978cb961543c0b65d50a83a591b6f60/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/peke/Documents/Koulu/FunktioOhjelmointi/luento12/.stack-work/install/x86_64-linux-tinfo6/f7b920acfb8162e6bc88428bb845ddcb5978cb961543c0b65d50a83a591b6f60/8.8.4/share/x86_64-linux-ghc-8.8.4/luento12-0.1.0.0"
libexecdir = "/home/peke/Documents/Koulu/FunktioOhjelmointi/luento12/.stack-work/install/x86_64-linux-tinfo6/f7b920acfb8162e6bc88428bb845ddcb5978cb961543c0b65d50a83a591b6f60/8.8.4/libexec/x86_64-linux-ghc-8.8.4/luento12-0.1.0.0"
sysconfdir = "/home/peke/Documents/Koulu/FunktioOhjelmointi/luento12/.stack-work/install/x86_64-linux-tinfo6/f7b920acfb8162e6bc88428bb845ddcb5978cb961543c0b65d50a83a591b6f60/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "luento12_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "luento12_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "luento12_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "luento12_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "luento12_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "luento12_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
