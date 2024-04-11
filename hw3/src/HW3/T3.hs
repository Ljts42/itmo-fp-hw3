module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import           HW3.T1

joinOption :: Option (Option a) -> Option a
joinOption None          = None
joinOption (Some option) = option

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error er)        = Error er
joinExcept (Success success) = success

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((value :# a1) :# a2) = value :# (a1 <> a2)

joinList :: List (List a) -> List a
joinList Nil = Nil
joinList (a :. as) = joinSmallList a where
    joinSmallList Nil       = joinList as
    joinSmallList (b :. bs) = b :. joinSmallList bs

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F $ \i -> get (f i) i where get (F g) i = g i
