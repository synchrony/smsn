{-
:! ghc -c SmSnLenses.hs -XFlexibleContexts
:load SmSnLenses
-}

module SmSnLenses where

type Id = String
type Role = String

data Entity = Entity Id Deriving Show
data RoleEntityPair = RoleEntityPair Role Entity deriving Show
data Statement = Statement [RoleEntityPair] deriving Show
data Dataset = [Statement] deriving Show



data Tree a = Tree a, [Tree a] deriving Show

data PropertyInView = Property { key :: String, value :: String } deriving Show
data EntityInView = EntityInContext { role :: Role, id :: Maybe Id, text :: String } deriving Show




child: Socrates
    mother: Phaenarete
    father: Sophroniscus


child:
  Socrates
    mother:
      Phaenarete
        father: Sophroniscus
      Betty Boop
        father: Popeye