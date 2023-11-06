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
mapOption _ None            = None
mapOption func (Some value) = Some $ func value

data Pair a = P a a
  deriving Show

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair func (P first second) = func first `P` func second

data Quad a = Q a a a a
  deriving Show

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad func (Q first second third fourth) = Q (func first) (func second) (func third) (func fourth)

data Annotated e a = a :# e
  deriving Show

infix 0 :#

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated func (value :# annotation) = func value :# annotation

data Except e a = Error e | Success a
  deriving Show

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error er)         = Error er
mapExcept func (Success value) = Success $ func value

data Prioritised a = Low a | Medium a | High a
  deriving Show

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised func (Low value)    = Low $ func value
mapPrioritised func (Medium value) = Medium $ func value
mapPrioritised func (High value)   = High $ func value

data Stream a = a :> Stream a
  deriving Show

infixr 5 :>

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream func (x :> xs) = func x :> mapStream func xs

data List a = Nil | a :. List a
  deriving Show

infixr 5 :.

mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil          = Nil
mapList func (x :. xs) = func x :. mapList func xs

data Fun i a = F (i -> a)

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun func (F g) = F $ \i -> func $ g i

data Tree a = Leaf | Branch (Tree a) a (Tree a)
  deriving Show

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf = Leaf
mapTree func (Branch left value right) = Branch (mapTree func left) (func value) (mapTree func right)
