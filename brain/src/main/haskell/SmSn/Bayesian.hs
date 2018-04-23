{-
Prerequisites:

cabal install random
cabal install MonadRandom
cabal install split

Text, e.g.:

cat /Users/josh/copy/humor/douglas_adams/hhgttg/book1.txt | grep -v "=\|Chapter" | tr '\r\n\t\"' ' ' | sed 's/  */ /g' > /tmp/bayes-play.txt

Compile and load:

:! ghc -c SmSn/Bayesian.hs
:load SmSn/Bayesian

SmSn.Bayesian.main
-}

module SmSn.Bayesian where

import Data.Map as M
import Data.Maybe as Y
import Data.List as L
import Control.Monad.Random as R
import Data.List.Split as S

type WeightedMap a b = Map a (Map b Int)

type BayesianMap m a = Map [a] (m a)


--staticText = "mississippi"

main = do
  --let inputSeq = staticText
  text <- readFile "/tmp/bayes-play.txt"
  let inputSeq = S.splitOn " " text
  let wm = createMap 2 inputSeq
      bms = bayesianMapWithStarts wm
  let showEl = \s -> " " ++ s
  beginOut showEl bms

beginOut showEl (bm, starts) = do
  putStr "\n"
  loopOut showEl (head starts) (bm, tail starts)

loopOut showEl left (bm, starts) = do
  Y.maybe (beginOut showEl (bm, starts)) (\r -> continueOut showEl left r (bm, starts)) (M.lookup left bm)

continueOut showEl left nr bms = do
  nextRight <- R.evalRandIO nr
  --putStr [nextRight]
  putStr (showEl nextRight)
  loopOut showEl ((tail left) ++ [nextRight]) bms


-- | constructs a network of weighted random transitions from input sequences to output elements
bayesianMapWithStarts :: (Ord a, MonadRandom m) => WeightedMap [a] a -> (BayesianMap m a, [[a]])
bayesianMapWithStarts wm = (bm, L.cycle $ M.keys bm)
  where convert theMap = R.fromList $ M.toList (fmap fromIntegral theMap)
        bm = fmap convert wm

-- | consumes a sequence such as a string of characters, producing a frequency map of substrings of a given length
createMap :: Ord a => Int -> [a] -> WeightedMap [a] a
createMap length seq = addToMap length seq (M.fromList [])

-- | creates, for a given input sequence, a map of prefixes of length n to maps of elements to integers
-- The inner map indicates how many times a given element follows a given sub-sequence.
addToMap :: Ord a => Int -> [a] -> WeightedMap [a] a -> WeightedMap [a] a
addToMap _ [] wm = wm
addToMap n seq wm = case nextPair n seq of
  Just (left, right) -> addToMap n (tail seq) (incPair left right wm)
  Nothing -> wm

-- | increments, in a WeightedMap, the weight on the association between a "left" value and a "right" value
incPair :: (Ord a, Ord b) => a -> b -> WeightedMap a b -> WeightedMap a b
incPair left right wm =
  let initial = case M.lookup left wm of
        Just theMap -> theMap
        Nothing -> M.fromList []
  in M.insert left (incCount initial right) wm

-- | increments the integer value for a given key in a map, where the default value is 0
incCount :: Ord a => Map a Int -> a -> Map a Int
incCount theMap key = M.insert key count theMap
  where count = 1 + (Y.fromMaybe 0 $ M.lookup key theMap)

-- | finds, in a list, a list of the next n elements together with the element immediately after them
nextPair :: Int -> [a] -> Maybe ([a], a)
nextPair _ [] = Nothing
nextPair 0 (x:xs) = Just ([], x)
nextPair n (x:xs) =
  case (nextPair (n - 1) xs) of
    Just (first, second) -> Just (x:first, second)
    Nothing -> Nothing
