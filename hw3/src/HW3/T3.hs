module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1

joinOption :: Option (Option a) -> Option a
joinOption = undefined

joinExcept :: Except e (Except e a) -> Except e a
joinExcept = undefined

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated = undefined

joinList :: List (List a) -> List a
joinList = undefined

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun = undefined
