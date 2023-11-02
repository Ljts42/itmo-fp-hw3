module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  ) where

import HW3.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption = undefined

wrapOption :: a -> Option a
wrapOption = undefined

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair = undefined

wrapPair :: a -> Pair a
wrapPair = undefined

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad = undefined

wrapQuad :: a -> Quad a
wrapQuad = undefined

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated = undefined

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated = undefined

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept = undefined

wrapExcept :: a -> Except e a
wrapExcept = undefined

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised = undefined

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = undefined

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream = undefined

wrapStream :: a -> Stream a
wrapStream = undefined

distList :: (List a, List b) -> List (a, b)
distList = undefined

wrapList :: a -> List a
wrapList = undefined

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun = undefined

wrapFun :: a -> Fun i a
wrapFun = undefined
