{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_haskell_bot (
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

bindir     = "C:\\Users\\isaac\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\isaac\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2\\haskell-bot-0.1.0.0-inplace-haskell-bot"
dynlibdir  = "C:\\Users\\isaac\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2"
datadir    = "C:\\Users\\isaac\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2\\haskell-bot-0.1.0.0"
libexecdir = "C:\\Users\\isaac\\AppData\\Roaming\\cabal\\haskell-bot-0.1.0.0-inplace-haskell-bot\\x86_64-windows-ghc-8.10.2\\haskell-bot-0.1.0.0"
sysconfdir = "C:\\Users\\isaac\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_bot_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_bot_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_bot_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_bot_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_bot_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_bot_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
