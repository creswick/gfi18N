module Main where

import Control.Monad (when)
import System.Environment ( getArgs )
import PGF ( readPGF, PGF, Language, readLanguage
           , linearize, languages )
import Foods

pgfFile :: FilePath
pgfFile = "dist/build/autogen/Foods.pgf"

-- | Define a template for a statement using the abstract GF syntax.
-- This has been converted to a tree of Haskell data types in
-- Foods.hs, which is generated by a custom Setup.hs
--
-- Each sort of statement will need a template somewhat like this.
-- Pass in the parameters that are identified at runtime (such as
-- numbers, genders, etc.).  The GF documentation helps to identify
-- the specifics, but in most cases this is handled by the grammars
-- defined in the 'gf' source tree that is parallel to the haskell
-- source.
--
-- This representation is independent of any languages.
--
-- The example here defines statements that look like:
--
--  "those <thing> are very fresh"      (Eng)
--  "quelle <thing> sono molto fresche" (Ita)
--
freshStatement :: GKind -> GComment
freshStatement thing = GPred (GThose thing) (GVery GFresh)


-- | Generate a string in a concrete language, such as English or Italian.
--
-- The third parameter (the GKind) specifies the object of this
-- phrase, and needs to be defined in the abstract syntax of
-- statements. (see gf/Foods.gf)
mkFreshStatement :: PGF -> Language -> GKind -> String
mkFreshStatement pgf lang item = linearize pgf lang (gf $ freshStatement item)

main :: IO ()
main = do
  args <- getArgs
  pgf <- readPGF pgfFile
  when (1 > length args) $ showLangsAndExit pgf
  let langStr = args!!0
      mbLang = readLanguage langStr
  case mbLang of
    Nothing -> fail ("Could not parse language: "++langStr)
    Just lang  -> putStrLn $ mkFreshStatement pgf lang GPizza


showLangsAndExit :: PGF -> IO ()
showLangsAndExit pgf = do
  mapM_ (putStrLn . show) $ languages pgf
  fail "Specify one of the above languages."
