{-
-- cabal install uuid
cabal install system-uuid

:! ghc -c SmSnIds.hs
:load SmSnIds
-}

module SmSnIds where

import Data.UUID
import System.Random

newUUID :: IO UUID
newUUID = randomIO

count = 300000

main = sequence_ $ replicate count $ do
         x <- newUUID
         y <- newUUID
         seq (x == y) $ return ()