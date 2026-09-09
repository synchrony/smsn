{-# LANGUAGE ScopedTypeVariables #-}

module Smsn.Terms.Fold where

-- The FOLD, written in the Hydra DSL rather than by hand in Python.
--
-- The fold takes the per-atom enriched records -- each a Node with no
-- children -- plus the parent/child structure, and produces one folded
-- Node tree per ENTITY, with each entity's non-entity descendants nested
-- inside it.
--
-- This is the first term-level module promoted out of joshkg's Python.
-- The Python original is packages/tools/pipeline/fold.py, which stays as
-- the driver: it reads SQLite, calls this logic, and writes node_json.
-- Only the pure core moves here.
--
-- WHY THIS ONE FIRST. Measured across the pipeline, fold.py is 261 lines
-- with 4% I/O, its core is a single recursive function, and -- unlike the
-- projector -- none of its rules are repairs for historical data damage.
-- It also does not mention Collection, so it is unaffected by the
-- hydra-java parametric-union decoder bug (CategoricalData/hydra#740).
--
-- WHAT IT IS NOT. This does not decide WHICH atoms are entities; that is
-- an input. It does not read or write anything. Given the same inputs it
-- returns the same trees.

import           Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Dsl.Annotations as Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Terms as Terms

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S


ns :: ModuleName
ns = ModuleName "net.fortytwo.smsn.fold"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = [],
            moduleMetadata = descriptionMetadata
              (Just "The fold: nest each entity's non-entity descendants inside it")}
  where
    definitions = [
      toDefinition foldableChildren]

-- | The one rule that makes the fold a fold.
--
-- Of an atom's children, exactly those that are NOT entities are folded
-- into it. Entity children are omitted here because they become their own
-- top-level trees; including them would duplicate every entity under each
-- of its parents, and the store is not a tree -- an atom can have several
-- parents, so duplication would be unbounded rather than merely wasteful.
--
-- A child absent from the entity map is treated as an entity, i.e. NOT
-- folded. That is the safe default: such a child lives in a source this
-- client cannot see, and inlining content from a hidden source into a
-- visible parent is exactly the disclosure the visibility model forbids.
-- Omitting it leaves a reference that resolves to nothing, which is the
-- honest outcome.
foldableChildren :: TypedTermDefinition (M.Map String Bool -> [String] -> [String])
foldableChildren = define "foldableChildren" $
  doc "Of an atom's children, those which are NOT entities and so fold \
      \into it. A child whose entity status is unknown -- because it \
      \lives in a source this client cannot see -- is treated as an \
      \entity and left unfolded, so hidden content is never inlined into \
      \a visible parent." $
  "isEntity" ~> "children" ~>
  Lists.filter
    ("child" ~> Optionals.match
      (Maps.lookup (var "child") (var "isEntity"))
      false                                  -- unknown -> not folded
      ("b" ~> Logic.not (var "b")))          -- entity  -> not folded
    (var "children")
