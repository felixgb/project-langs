module Paths_typed_arith (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/felixb/jra/project-langs/typed-arith/.cabal-sandbox/bin"
libdir     = "/home/felixb/jra/project-langs/typed-arith/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/typed-arith-0.1.0.0-6YltEBLyhP57zKK1WnU15j"
datadir    = "/home/felixb/jra/project-langs/typed-arith/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/typed-arith-0.1.0.0"
libexecdir = "/home/felixb/jra/project-langs/typed-arith/.cabal-sandbox/libexec"
sysconfdir = "/home/felixb/jra/project-langs/typed-arith/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "typed_arith_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "typed_arith_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "typed_arith_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "typed_arith_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "typed_arith_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
