module Smsn.Models.Brain017 where

-- The SmSn Brain model, ported from the hydra 0.7 DSL to 0.17.6.
--
-- The original is Smsn/Models/Brain.hs, which no longer compiles: it uses
-- `Hydra.Dsl.Bootstrap`, `datatype`/`typeref`, and the positional `Module`
-- constructor, all of which moved or changed. The TYPES it declares are
-- unchanged here -- this is a port of the idiom, not a redesign.
--
-- Why this matters beyond compilation: Atom is the input to the fold, and
-- Node (net.fortytwo.smsn.kg.instances.Node) is its output. While Atom was
-- stranded on 0.7 the two types could not appear in one Hydra generation,
-- so `fold : [Atom] -> [Node]` was not expressible at all. Porting Atom is
-- the prerequisite for writing any of the pipeline in the DSL rather than
-- by hand in Python.
--
-- Field optionality is not guesswork. Measured across all 169,693 atoms in
-- the current store: id, created, weight, source, title and children are
-- always present; priority is absent on 166,387, alias on 144,438, text on
-- 168,036 and shortcut on 169,673. That matches the original exactly, which
-- is good evidence the original was right.

import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations (doc)
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types as T


ns :: ModuleName
ns = ModuleName "net.fortytwo.smsn.brain"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = [],
            moduleMetadata = descriptionMetadata
              (Just "The SmSn Brain model: atoms and the tree views over them")}
  where
    definitions = [
      atom,
      atomId,
      normed,
      sourceName,
      timestamp,
      treeNode]

-- ====================================================================
-- Primitives
-- ====================================================================

atomId :: TypeDefinition
atomId = define "AtomId" $
  doc "The unique id of an atom, as a base-62 encoded number" $
  T.wrap T.string

normed :: TypeDefinition
normed = define "Normed" $
  doc "A normalized floating-point value representing weight or \
      \probability, ranging from 0.0 to 1.0" $
  T.wrap T.float32

sourceName :: TypeDefinition
sourceName = define "SourceName" $
  doc "The name of a source: the sub-corpus an atom belongs to, and the \
      \unit of visibility. A source may be hidden from a given client, in \
      \which case its atoms' content is withheld while their ids remain \
      \legitimate reference targets." $
  T.wrap T.string

timestamp :: TypeDefinition
timestamp = define "Timestamp" $
  doc "A Unix timestamp in milliseconds" $
  T.wrap T.int64

-- ====================================================================
-- Atoms and trees
-- ====================================================================

atom :: TypeDefinition
atom = define "Atom" $
  doc "A raw SmSn atom: the unit of the store as the user authored it, \
      \prior to any enrichment or typing. Distinct from \
      \net.fortytwo.smsn.kg.instances.Node, which is the ENRICHED, folded \
      \form carrying a TypedInstance; the fold reads Atoms and produces \
      \Nodes." $
  T.record [
    "id">:
      doc "The atom's identifier, unique within the store" $
      atomId,
    "created">:
      doc "Creation timestamp, milliseconds since the epoch" $
      timestamp,
    "weight">:
      doc "Importance in [0,1]; drives filtering and display" $
      normed,
    "priority">:
      doc "Optional priority in [0,1], for atoms on a to-do list" $
      T.optional normed,
    "source">:
      doc "The sub-corpus this atom belongs to (e.g. data-public, personal)" $
      sourceName,
    "title">:
      doc "The atom's title: its primary human-readable content"
      T.string,
    "alias">:
      doc "An optional external identifier, typically a URL" $
      T.optional T.string,
    "text">:
      doc "Optional body text, where the atom carries more than a title" $
      T.optional T.string,
    "shortcut">:
      doc "An optional user-defined short name for quick navigation" $
      T.optional T.string,
    "children">:
      doc "The atom's children, in author-specified order. A child in a \
          \hidden source resolves to nothing for a client that cannot see \
          \it; that is NOT the same as a dangling reference, which is an \
          \error." $
      T.list atomId]

treeNode :: TypeDefinition
treeNode = define "TreeNode" $
  doc "An atom together with its children, as exchanged with a client. \
      \The same fields as Atom, but `children` holds whole TreeNodes \
      \rather than ids -- this is the shape smsn-mode renders." $
  T.record [
    "id">:
      doc "The atom's identifier" $
      atomId,
    "created">:
      doc "Creation timestamp, milliseconds since the epoch" $
      timestamp,
    "weight">:
      doc "Importance in [0,1]" $
      normed,
    "priority">:
      doc "Optional priority in [0,1]" $
      T.optional normed,
    "source">:
      doc "The sub-corpus this atom belongs to" $
      sourceName,
    "title">:
      doc "The atom's title"
      T.string,
    "alias">:
      doc "An optional external identifier" $
      T.optional T.string,
    "text">:
      doc "Optional body text" $
      T.optional T.string,
    "shortcut">:
      doc "An optional user-defined short name" $
      T.optional T.string,
    "children">:
      doc "The child nodes, inline and in author order" $
      T.list treeNode]
