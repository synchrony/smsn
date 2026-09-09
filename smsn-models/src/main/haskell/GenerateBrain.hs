-- | Generate the SmSn Brain model (Atom, TreeNode, ...) to a target language.
--
-- Proves the DSL -> multi-language round trip on a TYPE module before any
-- term module is attempted. Output dir from SMSN_BRAIN_OUT.
module Main where

import qualified Smsn.Models.Brain017 as Brain
import           Hydra.ExtGeneration (writePython, writeJava)
import           Hydra.Sources.All   (kernelModules)
import           System.Environment  (lookupEnv)
import           Data.Maybe          (fromMaybe)

main :: IO ()
main = do
  outDir <- fromMaybe "out" <$> lookupEnv "SMSN_BRAIN_OUT"
  let mods = [Brain.module_]
  putStrLn $ "Generating " ++ show (length mods) ++ " module(s) to " ++ outDir
  pj <- writePython (outDir ++ "/python") (kernelModules ++ mods) mods
  putStrLn $ "  python: " ++ show (length pj) ++ " file(s)"
  mapM_ (putStrLn . ("    " ++)) pj
  jj <- writeJava (outDir ++ "/java") (kernelModules ++ mods) mods
  putStrLn $ "  java:   " ++ show (length jj) ++ " file(s)"
  mapM_ (putStrLn . ("    " ++)) jj
