{-# LANGUAGE CPP #-}
import Data.List ( intersperse, sort )

import Control.Monad (when)

import Distribution.Simple
import Distribution.Simple.Program.Types
import Distribution.Simple.Setup
import Distribution.Simple.UserHooks
import Distribution.Simple.Utils ( rawSystemExit, warn, debug
                                 , findProgramVersion, notice )
import Distribution.Verbosity ( Verbosity )

import System.Directory ( getCurrentDirectory, setCurrentDirectory,
                          createDirectoryIfMissing, removeFile,
                          doesFileExist, getDirectoryContents,
                          getModificationTime )
import System.Cmd       ( rawSystem )

main = do
  defaultMainWithHooks simpleUserHooks
       { preBuild = \a b -> generatePGF a b >> preBuild simpleUserHooks a b
       , preClean = \a b -> preClean simpleUserHooks a b
       }

 -- {-# OPTIONS -cpp #-}
#ifdef WIN32
pathSeparator = "\\"
#else
pathSeparator = "/"
#endif

-- We can't depend on FilePath because we're in a Setup.hs
(</>) :: FilePath -> FilePath -> FilePath
(</>) a b = a ++ pathSeparator ++ b

mkPath :: [FilePath] -> FilePath
mkPath (f:fs) = foldl (</>) f fs

gfExecutable :: IO FilePath
gfExecutable = return "gf"

generatePGF :: Args -> BuildFlags -> IO ()
generatePGF _ flags = do
  orig <- getCurrentDirectory
  let verbosity = (fromFlag $ buildVerbosity flags)
      autogenDir = mkPath [orig, "dist", "build", "autogen"]
      gfSrcDir = mkPath [orig, "src", "gf"]
      profileName = "Foods"
      profileFlavors = [ "Eng", "Ita" ]
      pgfFile = autogenDir </> profileName++".pgf"

  notice verbosity "checking for dirty GF files to build"
  doBuild <- needsBuild gfSrcDir pgfFile
  when doBuild $ do
    createDirectoryIfMissing True autogenDir
    notice verbosity "Running PGF build"

    setCurrentDirectory gfSrcDir
    gfExe <- gfExecutable
    setCurrentDirectory autogenDir

    -- create the PGF files:
    rawSystemExit verbosity gfExe $
                     [ "--make"
                     , "--no-emit-gfo"
                     , "--gfo-dir="++autogenDir
                     ] ++ [ gfSrcDir </> (profileName ++ f ++ ".gf") |
                                f <- profileFlavors
                          ]

    -- generate the haskell:
    rawSystemExit verbosity gfExe [ "--batch"
                                  , "--no-emit-gfo"
                                  , "--output-format=haskell"
                                  , "--output-dir="++autogenDir
                                  , profileName++".pgf"  -- hmm
                                  ]
    setCurrentDirectory orig

needsBuild :: FilePath -> FilePath -> IO Bool
needsBuild dir f = do exists <- doesFileExist f
                      case exists of
                        False -> return True
                        True  -> do
                          files <- getDirectoryContents dir
                          res <- mapM (\x->compareTimes (dir </> x) f) (filter (".."/=) files)
                          return $ any (GT==) res

compareTimes :: FilePath -> FilePath -> IO Ordering
compareTimes f1 f2 = do t1 <- getModificationTime f1
                        t2 <- getModificationTime f2
                        return $ compare t1 t2
