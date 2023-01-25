{-# LANGUAGE OverloadedStrings #-}

module Smsn.Models.Brain where

import Hydra.Kernel
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard
import Hydra.Impl.Haskell.Sources.Core


smsnModules = [smsnBrainModule]

smsnBrainModule :: Module Meta
smsnBrainModule = Module ns elements [] Nothing
  where
    ns = Namespace "net/fortytwo/smsn/brain"
    brain = nsref ns

    def = datatype ns

    elements = [
--      def "Role" $
--        enum ["entity", "relation"]]

      def "AtomId" string,

      def "Atom" $
        record [
          "id">: brain "AtomId",
          "created">: brain "Timestamp",
          "weight">: brain "Weight",
          "priority">: optional $ brain "Priority",
          "source">: brain "SourceName",
          "title">: string,
          "alias">: optional string,
          "children">: list $ brain "AtomId"],

      def "Priority" float32,

      def "SourceName" string,

      def "Timestamp" $
        doc "A Unix timestamp in seconds"
        int32,

--      def "TreeNode" $
--        record [
--          "id">: brain "AtomId",
--          "created">: brain "Timestamp",
--          "weight">: brain "Weight",
--          "priority">: optional $ brain "Priority",
--          "source">: brain "SourceName",
--          "title">: string,
--          "alias">: optional string,
--          "children">: list $ brain "TreeNode",
--          "numberOfChildren">: int32,
--          "numberOfParents">: int32,
--        ],
--
--      def "TreeView" $
--        record [
--          "root">: brain "AtomId",
--          "title">: string,
--          "view">: brain "TreeNode",
--          "minSource">: brain "SourceName",
--          "defaultSource">: brain "SourceName",
--          "minWeight">: brain "Weight",
--          "defaultWeight">: brain "Weight"],

      def "Weight" float32]
