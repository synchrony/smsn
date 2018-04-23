{-
:! ghc -c SmSn/Ripple.hs
:load SmSn/Ripple

SmSn.Ripple.main


reduce [Op, dup, string "foo"]
reduce [Op, plus, int 2, int 3, int 42]
reduce [Op, swap, int 2, int 3, int 42]
reduce [Op, times, int 3, ListValue [int 100, int 50], string "foo"]
reduce [Op, times, int 2, ListValue [dup, Op], string "foo"]
reduce [Op, SmSn.Ripple.sqrt, double 42]


[Op, times, int 2, ListValue [dup, Op], string "foo"]
[Op, ListValue [dup, Op], Op, ListValue [dup, Op], string "foo"]
[Op, dup, Op, ListValue [dup, Op], string "foo"]


-}

module SmSn.Ripple where

import Data.List as L
import Data.Maybe as Y

-- data types ------------------------------------------------------------------

data Value = Op
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

reduce :: Stack -> [Stack]
-- an Op with nothing beneath it has no reduction
reduce [Op] = []
-- an Op at the head of a non-empty stack will activate the element beneath it
reduce (Op:tail) = reduceAll $ activate (L.head tail) (L.tail tail)
-- all other stacks, including the empty stack, remain unchanged
reduce xs = [xs]

reduceAll :: [Stack] -> [Stack]
reduceAll stacks = L.concat $ L.map reduce stacks

activate :: Value -> Stack -> [Stack]
activate Op _ = []
activate (MappingValue mv) tail = applyMapping mv tail
activate (ListValue lv) tail = [(reverse lv) ++ tail]
activate head tail = [head:tail]

applyMapping :: Mapping -> Stack -> [Stack]
applyMapping (Mapping inArity outArity mapping) stack = Y.maybe [] apply $ marshal inArity (stack, [])
  where apply = \(tail, args) -> prependAll tail $ mapping args

prependAll :: Stack -> [Stack] -> [Stack]
prependAll tail stacks = L.map (\s -> s ++ tail) stacks

marshal :: Int -> ([a], [a]) -> Maybe ([a], [a])
marshal 0 stacks = Just stacks
marshal _ ([], _) = Nothing
marshal n (x:xs, second) = Y.maybe Nothing psh $ marshal (n-1) (xs, second)
  where psh = \(first', second') -> Just (first', (x:second'))

-- a small library of mappings -------------------------------------------------

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
  where mapping [(IntValue n), v] = [L.concat $ take n $ repeat [Op, v]]
        mapping _ = []
