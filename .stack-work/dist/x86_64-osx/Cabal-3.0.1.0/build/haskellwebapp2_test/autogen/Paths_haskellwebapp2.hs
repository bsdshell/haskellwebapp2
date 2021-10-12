{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_haskellwebapp2 (
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

bindir     = "/Users/aaa/myfile/bitbucket/haskellwebapp2/.stack-work/install/x86_64-osx/5da05f56ea89a5a5e16df9b0bb3f39595ac4c68d13eccc0c88442113664db12f/8.8.4/bin"
libdir     = "/Users/aaa/myfile/bitbucket/haskellwebapp2/.stack-work/install/x86_64-osx/5da05f56ea89a5a5e16df9b0bb3f39595ac4c68d13eccc0c88442113664db12f/8.8.4/lib/x86_64-osx-ghc-8.8.4/haskellwebapp2-0.1.0.0-CUQvMCHgLpJJTTVwLY2M4B-haskellwebapp2_test"
dynlibdir  = "/Users/aaa/myfile/bitbucket/haskellwebapp2/.stack-work/install/x86_64-osx/5da05f56ea89a5a5e16df9b0bb3f39595ac4c68d13eccc0c88442113664db12f/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/aaa/myfile/bitbucket/haskellwebapp2/.stack-work/install/x86_64-osx/5da05f56ea89a5a5e16df9b0bb3f39595ac4c68d13eccc0c88442113664db12f/8.8.4/share/x86_64-osx-ghc-8.8.4/haskellwebapp2-0.1.0.0"
libexecdir = "/Users/aaa/myfile/bitbucket/haskellwebapp2/.stack-work/install/x86_64-osx/5da05f56ea89a5a5e16df9b0bb3f39595ac4c68d13eccc0c88442113664db12f/8.8.4/libexec/x86_64-osx-ghc-8.8.4/haskellwebapp2-0.1.0.0"
sysconfdir = "/Users/aaa/myfile/bitbucket/haskellwebapp2/.stack-work/install/x86_64-osx/5da05f56ea89a5a5e16df9b0bb3f39595ac4c68d13eccc0c88442113664db12f/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskellwebapp2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskellwebapp2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskellwebapp2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskellwebapp2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskellwebapp2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskellwebapp2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
