{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_harjoitus2postiOpe (
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

bindir     = "/home/peke/Documents/Koulu/haskell-luento-6/harjoitus2postiOpe/.stack-work/install/x86_64-linux/e3e50bd7fad6e3a6da95bd076e6ac94d29874efa6ede94bc9fead457c5cabe8a/8.8.4/bin"
libdir     = "/home/peke/Documents/Koulu/haskell-luento-6/harjoitus2postiOpe/.stack-work/install/x86_64-linux/e3e50bd7fad6e3a6da95bd076e6ac94d29874efa6ede94bc9fead457c5cabe8a/8.8.4/lib/x86_64-linux-ghc-8.8.4/harjoitus2postiOpe-0.1.0.0-Ln3HVGnkfbaA0gQgpNB0Fz-harjoitus2postiOpe"
dynlibdir  = "/home/peke/Documents/Koulu/haskell-luento-6/harjoitus2postiOpe/.stack-work/install/x86_64-linux/e3e50bd7fad6e3a6da95bd076e6ac94d29874efa6ede94bc9fead457c5cabe8a/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/peke/Documents/Koulu/haskell-luento-6/harjoitus2postiOpe/.stack-work/install/x86_64-linux/e3e50bd7fad6e3a6da95bd076e6ac94d29874efa6ede94bc9fead457c5cabe8a/8.8.4/share/x86_64-linux-ghc-8.8.4/harjoitus2postiOpe-0.1.0.0"
libexecdir = "/home/peke/Documents/Koulu/haskell-luento-6/harjoitus2postiOpe/.stack-work/install/x86_64-linux/e3e50bd7fad6e3a6da95bd076e6ac94d29874efa6ede94bc9fead457c5cabe8a/8.8.4/libexec/x86_64-linux-ghc-8.8.4/harjoitus2postiOpe-0.1.0.0"
sysconfdir = "/home/peke/Documents/Koulu/haskell-luento-6/harjoitus2postiOpe/.stack-work/install/x86_64-linux/e3e50bd7fad6e3a6da95bd076e6ac94d29874efa6ede94bc9fead457c5cabe8a/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "harjoitus2postiOpe_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "harjoitus2postiOpe_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "harjoitus2postiOpe_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "harjoitus2postiOpe_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "harjoitus2postiOpe_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "harjoitus2postiOpe_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
