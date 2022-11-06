{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hask_lox (
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

bindir     = "C:\\Users\\Christoph\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Christoph\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.5\\hask-lox-0.1.0.0-inplace-hask-lox"
dynlibdir  = "C:\\Users\\Christoph\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\Christoph\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.5\\hask-lox-0.1.0.0"
libexecdir = "C:\\Users\\Christoph\\AppData\\Roaming\\cabal\\hask-lox-0.1.0.0-inplace-hask-lox\\x86_64-windows-ghc-8.6.5\\hask-lox-0.1.0.0"
sysconfdir = "C:\\Users\\Christoph\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hask_lox_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hask_lox_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hask_lox_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hask_lox_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hask_lox_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hask_lox_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
