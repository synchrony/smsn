{-
:! ghc -c SmSn/Ripple.hs
:load SmSn/Ripple

SmSn.Ripple.main

Examples:

reduce 1 [Do, dup, string "foo"]
reduce 1 [Do, plus, int 2, int 3, int 42]
reduce 1 [Do, swap, int 2, int 3, int 42]
reduce 2 [Do, times, int 3, ListValue [int 100, int 50], string "foo"]
reduce 4 [Do, times, int 3, ListValue [int 100, int 50], string "foo"]
reduce 6 [Do, times, int 3, ListValue [int 100, int 50], string "foo"]
reduce 1 [Do, times, int 2, ListValue [dup, Do], string "foo"]
reduce 1 [Do, SmSn.Ripple.sqrt, double 42]
-}

module SmSn.Ripple where

import Data.List as L
import Data.Maybe as Y

-- data types ------------------------------------------------------------------

data Value = Do
         | BooleanValue Bool
         | DoubleValue Double
         | IntValue Int
         | ListValue [Value]
         | MappingValue Mapping
         | StringValue String deriving Show

data Mapping = Mapping Int Int (Stack -> [Stack])
instance Show Mapping where
  show (Mapping inArity outArity _)
    = "(" ++ show inArity ++ "->" ++ show outArity ++ ")"

type Stack = [Value]

type StackSet = [Stack]

bool = BooleanValue
int = IntValue
double = DoubleValue
list = ListValue
string = StringValue

-- reduction/computation -------------------------------------------------------

-- | reduces a stack to N-normal form; a stack is in N-normal form if it has at least N elements and none of the
--   first N elements is "Do"
reduce :: Int -> Stack -> [Stack]
-- every stack is in 0-normal form
reduce 0 stack = [stack]
-- a Do with nothing beneath it has no N>0 normal form
reduce _ [Do] = []
-- a Do with elements beneath it first reduces the tail of the stack to 1st normal form,
-- pops a mapping value from each resulting stack, and applies the mapping to the tail of each resulting stack
reduce 1 (Do:tail) = forAll (reduce 1) $ forAll popAndApply $ reduce 1 tail
  where popAndApply s = apply (L.head s) (L.tail s)
-- any stack without a Do at its head, including the empty stack, remains unchanged at a depth of 1
reduce 1 xs = [xs]
-- at depths greater than one, we must first reduce the stack to a depth of one,
-- then remove the heads of the new stacks, then reduce the tails of the stacks to a depth of N-1,
-- and finally push the original heads back to the N-1 reduced stacks
reduce n stack = forAll reduceTail $ reduce 1 stack
  where reduceTail [] = []
        reduceTail [_] = []
        reduceTail (head:tail) = map (\s -> head:s) $ reduce (n-1) tail

-- | removes the head of the stack and applies it to the tail of the stack according to a type-specific rule
apply :: Value -> Stack -> [Stack]
-- a mapping is applied according to its internal lambda
apply (MappingValue mv) tail = reduceAndApply mv tail
-- a list is applied by dequoting the list and prepending it to the tail of the stack
apply (ListValue lv) tail = [(reverse lv) ++ tail]
-- other values cannot be applied, or rather their application is the null mapping
apply _ _ = []

-- | applies a mapping to a stack, first reducing the stack to N-normal form, where N is the mapping's in-arity
reduceAndApply :: Mapping -> Stack -> [Stack]
reduceAndApply (Mapping n _ mapping) stack = forAll (applyMapping n mapping) $ reduce n stack

-- | applies a mapping to a stack in N-normal form
applyMapping :: Int -> (Stack -> [Stack]) -> Stack -> [Stack]
applyMapping n mapping reduced = L.map (\s -> s ++ rest) $ mapping $ take n reduced
  where rest = drop n reduced

-- | a convenience function to distribute a given stack function across a list of stacks, concatenating the results
forAll :: (Stack -> [Stack]) -> [Stack] -> [Stack]
forAll f stacks = L.concat $ L.map f stacks

-- an example library of mappings ----------------------------------------------

plus = MappingValue $ Mapping 2 1 mapping
  where mapping [(IntValue v1), (IntValue v2)] = [[int $ v1 + v2]]
        mapping _ = []

sqrt = MappingValue $ Mapping 1 1 mapping
  where mapping [(DoubleValue v)] = if (v == 0) then [[double 0]] else
          if (v < 0) then [] else [[double (-result)], [double result]]
            where result = Prelude.sqrt v

swap = MappingValue $ Mapping 2 2 mapping
  where mapping [a, b] = [[b, a]]
        mapping _ = []

dup = MappingValue $ Mapping 1 2 mapping
  where mapping [a] = [[a, a]]
        mapping _ = []

times = MappingValue $ Mapping 2 0 mapping
  where mapping [(IntValue n), v] = [L.concat $ take n $ repeat [Do, v]]
        mapping _ = []
