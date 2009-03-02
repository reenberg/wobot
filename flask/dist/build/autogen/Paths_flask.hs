module Paths_flask (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/dybber/.cabal/bin"
libdir     = "/home/dybber/.cabal/lib/flask-0.1/ghc-6.10.1"
datadir    = "/home/dybber/.cabal/share/flask-0.1"
libexecdir = "/home/dybber/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "flask_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "flask_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "flask_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "flask_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
