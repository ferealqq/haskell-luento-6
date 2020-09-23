{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_luento8 (
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

bindir     = "/home/peke/Documents/Koulu/haskell-luento-6/luento8/.stack-work/install/x86_64-linux/e3e50bd7fad6e3a6da95bd076e6ac94d29874efa6ede94bc9fead457c5cabe8a/8.8.4/bin"
libdir     = "/home/peke/Documents/Koulu/haskell-luento-6/luento8/.stack-work/install/x86_64-linux/e3e50bd7fad6e3a6da95bd076e6ac94d29874efa6ede94bc9fead457c5cabe8a/8.8.4/lib/x86_64-linux-ghc-8.8.4/luento8-0.1.0.0-3qYxviIoY2nFzsCJD5vn5c-luento8"
dynlibdir  = "/home/peke/Documents/Koulu/haskell-luento-6/luento8/.stack-work/install/x86_64-linux/e3e50bd7fad6e3a6da95bd076e6ac94d29874efa6ede94bc9fead457c5cabe8a/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/peke/Documents/Koulu/haskell-luento-6/luento8/.stack-work/install/x86_64-linux/e3e50bd7fad6e3a6da95bd076e6ac94d29874efa6ede94bc9fead457c5cabe8a/8.8.4/share/x86_64-linux-ghc-8.8.4/luento8-0.1.0.0"
libexecdir = "/home/peke/Documents/Koulu/haskell-luento-6/luento8/.stack-work/install/x86_64-linux/e3e50bd7fad6e3a6da95bd076e6ac94d29874efa6ede94bc9fead457c5cabe8a/8.8.4/libexec/x86_64-linux-ghc-8.8.4/luento8-0.1.0.0"
sysconfdir = "/home/peke/Documents/Koulu/haskell-luento-6/luento8/.stack-work/install/x86_64-linux/e3e50bd7fad6e3a6da95bd076e6ac94d29874efa6ede94bc9fead457c5cabe8a/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "luento8_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "luento8_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "luento8_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "luento8_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "luento8_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "luento8_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
