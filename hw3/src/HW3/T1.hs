module HW3.T1
  ( Option (..)
  , Pair (..)
  , Quad (..)
  , Annotated (..)
  , Except (..)
  , Prioritised (..)
  , Stream (..)
  , List (..)
  , Fun (..)
  , Tree (..)
  , mapOption
  , mapPair
  , mapQuad
  , mapAnnotated
  , mapExcept
  , mapPrioritised
  , mapStream
  , mapList
  , mapFun
  , mapTree
  ) where

data Option a = None | Some a
  deriving Show

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption = undefined

data Pair a = P a a
  deriving Show

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair = undefined

data Quad a = Q a a a a
  deriving Show

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad = undefined

data Annotated e a = a :# e
  deriving Show

infix 0 :#

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated = undefined

data Except e a = Error e | Success a
  deriving Show

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept = undefined

data Prioritised a = Low a | Medium a | High a
  deriving Show

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised = undefined

data Stream a = a :> Stream a
  deriving Show

infixr 5 :>

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream = undefined

data List a = Nil | a :. List a
  deriving Show

infixr 5 :.

mapList :: (a -> b) -> (List a -> List b)
mapList = undefined

data Fun i a = F (i -> a)

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun = undefined

data Tree a = Leaf | Branch (Tree a) a (Tree a)
  deriving Show

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree = undefined
