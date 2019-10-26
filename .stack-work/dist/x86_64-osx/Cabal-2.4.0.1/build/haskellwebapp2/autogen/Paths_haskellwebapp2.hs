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

bindir     = "/Users/cat/myfile/bitbucket/haskellwebapp2/.stack-work/install/x86_64-osx/aedf13f238a04ec38eaea700275a5944789a6b76075efd85d470a4bda30235de/8.6.5/bin"
libdir     = "/Users/cat/myfile/bitbucket/haskellwebapp2/.stack-work/install/x86_64-osx/aedf13f238a04ec38eaea700275a5944789a6b76075efd85d470a4bda30235de/8.6.5/lib/x86_64-osx-ghc-8.6.5/haskellwebapp2-0.1.0.0-9f3ZmCm6jlaFSwbn17Lcoa-haskellwebapp2"
dynlibdir  = "/Users/cat/myfile/bitbucket/haskellwebapp2/.stack-work/install/x86_64-osx/aedf13f238a04ec38eaea700275a5944789a6b76075efd85d470a4bda30235de/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/cat/myfile/bitbucket/haskellwebapp2/.stack-work/install/x86_64-osx/aedf13f238a04ec38eaea700275a5944789a6b76075efd85d470a4bda30235de/8.6.5/share/x86_64-osx-ghc-8.6.5/haskellwebapp2-0.1.0.0"
libexecdir = "/Users/cat/myfile/bitbucket/haskellwebapp2/.stack-work/install/x86_64-osx/aedf13f238a04ec38eaea700275a5944789a6b76075efd85d470a4bda30235de/8.6.5/libexec/x86_64-osx-ghc-8.6.5/haskellwebapp2-0.1.0.0"
sysconfdir = "/Users/cat/myfile/bitbucket/haskellwebapp2/.stack-work/install/x86_64-osx/aedf13f238a04ec38eaea700275a5944789a6b76075efd85d470a4bda30235de/8.6.5/etc"

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
