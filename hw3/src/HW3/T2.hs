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

import           HW3.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some first, Some second) = Some (first, second)
distOption _                         = None

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P f1 s1, P f2 s2) = P (f1, f2) (s1, s2)

wrapPair :: a -> Pair a
wrapPair value = P value value

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 b1 c1 d1, Q a2 b2 c2 d2) = Q (a1, a2) (b1, b2) (c1, c2) (d1, d2)

wrapQuad :: a -> Quad a
wrapQuad value = Q value value value value

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (v1 :# a1, v2 :# a2) = (v1, v2) :# a1 <> a2

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated value = value :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error er, _)            = Error er
distExcept (_, Error er)            = Error er
distExcept (Success v1, Success v2) = Success (v1, v2)

wrapExcept :: a -> Except e a
wrapExcept = Success

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low first,    Low second)    = Low (first, second)
distPrioritised (Low first,    Medium second) = Medium (first, second)
distPrioritised (Low first,    High second)   = High (first, second)
distPrioritised (Medium first, Low second)    = Medium (first, second)
distPrioritised (Medium first, Medium second) = Medium (first, second)
distPrioritised (Medium first, High second)   = High (first, second)
distPrioritised (High first,   Low second)    = High (first, second)
distPrioritised (High first,   Medium second) = High (first, second)
distPrioritised (High first,   High second)   = High (first, second)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> as, b :> bs) = (a, b) :> distStream (as, bs)

wrapStream :: a -> Stream a
wrapStream value = value :> wrapStream value

distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (a :. as, list) = reduce list where
  reduce Nil       = distList (as, list)
  reduce (b :. bs) = (a, b) :. reduce bs

wrapList :: a -> List a
wrapList value = value :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F $ \i -> (f i, g i)

wrapFun :: a -> Fun i a
wrapFun value = F $ const value
