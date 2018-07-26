
module Applicative


-- Attempt to write applicative from memory
interface Functor s => MyApplicative (s : Type -> Type) where
          myPure : a -> s a
          myApply : s (a -> b) -> s a -> s b

