-- | Generate the SmSn Brain model (Atom, TreeNode, ...) to a target language.
--
-- Proves the DSL -> multi-language round trip on a TYPE module before any
-- term module is attempted. Output dir from SMSN_BRAIN_OUT.
module Main where

import qualified Smsn.Models.Brain017 as Brain
import qualified Smsn.Terms.Fold as Fold
import           Hydra.ExtGeneration (writePython, writeJava)
import           Hydra.Sources.All   (kernelModules)
import           Hydra.Generation    (generateDecoderModules, generateEncoderModules)
import           Hydra.Sources.Kernel.Types.All (kernelTypesModules)
import           System.Environment  (lookupEnv)
import           Data.Maybe          (fromMaybe)

main :: IO ()
main = do
  outDir <- fromMaybe "out" <$> lookupEnv "SMSN_BRAIN_OUT"
  let mods = [Brain.module_, Fold.module_]
  -- A TERM module's generated code calls the kernel's derived coders
  -- (hydra.decode.core.*, hydra.encode.core.*). Those are generated, not
  -- authored, so they are absent from kernelModules and must be
  -- synthesized into the universe -- for RESOLUTION only; they are never
  -- written out. Same fix GenerateEncoders.hs needed in joshkg.
  kd <- generateDecoderModules kernelModules kernelTypesModules
  ke <- generateEncoderModules kernelModules kernelTypesModules
  let universe = kernelModules ++ kd ++ ke ++ mods
  putStrLn $ "Generating " ++ show (length mods) ++ " module(s) to " ++ outDir
  pj <- writePython (outDir ++ "/python") universe mods
  putStrLn $ "  python: " ++ show (length pj) ++ " file(s)"
  mapM_ (putStrLn . ("    " ++)) pj
  jj <- writeJava (outDir ++ "/java") universe mods
  putStrLn $ "  java:   " ++ show (length jj) ++ " file(s)"
  mapM_ (putStrLn . ("    " ++)) jj
