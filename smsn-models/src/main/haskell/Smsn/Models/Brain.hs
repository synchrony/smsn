{-# LANGUAGE OverloadedStrings #-}

module Smsn.Models.Brain where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Sources.Core
import Hydra.Dsl.Types as Types


smsnModules = [smsnBrainModule]

smsnBrainModule :: Module Kv
smsnBrainModule = Module ns elements [hydraCoreModule] Nothing
  where
    ns = Namespace "net/fortytwo/smsn/brain"
    brain = typeref ns

    def = datatype ns

    elements = [
--      def "Role" $
--        enum ["entity", "relation"]]

      def "AtomId"
        doc "The unique id of an atom, as a base-62 encoded number"
        string,

      def "Atom" $
        record [
          "id">: brain "AtomId",
          "created">: brain "Timestamp",
          "weight">: brain "Normed",
          "priority">: optional $ brain "Normed",
          "source">: brain "SourceName",
          "title">: string,
          "alias">: optional string,
          "children">: list $ brain "AtomId"],

      def "Normed"
        doc "A normalized floating-point value representing weight or probability, ranging from 0.0 to 1.0"
        float32,

      def "SourceName" string,

      def "Timestamp" $
        doc "A Unix timestamp in seconds"
        int32,

      def "TreeNode" $
        record [
          "id">: brain "AtomId",
          "created">: brain "Timestamp",
          "weight">: brain "Normed",
          "priority">: optional $ brain "Normed",
          "source">: brain "SourceName",
          "title">: string,
          "alias">: optional string,
          "children">: list $ brain "TreeNode",
          "numberOfChildren">: int32,
          "numberOfParents">: int32]

--      def "TreeView" $
--        record [
--          "root">: brain "AtomId",
--          "title">: string,
--          "view">: brain "TreeNode",
--          "minSource">: brain "SourceName",
--          "defaultSource">: brain "SourceName",
--          "minWeight">: brain "Normed",
--          "defaultWeight">: brain "Normed"],

      ]
