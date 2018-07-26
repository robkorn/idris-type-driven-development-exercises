
module Functor

-- Functor typeclasee from memory
interface MyFunctor (s : Type -> Type) where
          fmap : s (a -> b) -> s a -> s b
