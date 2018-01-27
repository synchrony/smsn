{-
:! ghc -c SmSnDataModel.hs
:load SmSnDataModel
-}

module SmSnDataModel where

import Data.Hashable
import Data.HashMap.Strict

-- data model --------------------------

type Id = String
data Entity = Entity Id deriving Show

type RoleName = String
data Role = Role RoleName deriving Show
instance Eq Role where
    (Role x) == (Role y) = x == y
instance Hashable Role where
  hashWithSalt s (Role name) = s + (hash name)

data RoleEntityPair = RoleEntityPair Role Entity deriving Show
data Statement = Statement (HashMap Role Entity) deriving Show
data Dataset = Dataset [Statement] deriving Show

-- constructors ------------------------

pair k v = (Role k, Entity v)
statement pairs = Statement (fromList pairs)

-- examples ----------------------------

s0a = statement []

s0b = statement [
  pair "mother" "Alcmene"]

s1 = statement [
  pair "mother" "Rhea",
  pair "father" "Cronus",
  pair "child" "Zeus"]

s2 = statement [
  pair "mother" "Alcmene",
  pair "father" "Zeus",
  pair "child" "Hercules"]

s3 = statement [
  pair "child" "Athena",
  pair "father" "Zeus"]

s4 = statement [
  pair "child" "Ares",
  pair "father" "Zeus",
  pair "mother" "Hera"]

s5 = statement [
  pair "child" "Hebe",
  pair "father" "Zeus",
  pair "mother" "Hera"]

s6 = statement [
  pair"child" "Hephaestus",
  pair "father" "Zeus",
  pair "mother" "Hera"]

ds0 = Dataset [s1, s2, s3, s4, s5, s6]
