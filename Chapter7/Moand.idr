
module Monad

-- Attempt to write monad from memory with default implementations
interface Applicative m => MyMonad (m : Type -> Type) where
          myBind : m a -> (a -> m b) -> m b
          myJoin : m (m a) -> m a
          myJoin s = myBind s id
          myBind s func = myJoin $ func <$> s

MyMonad Maybe where
  myBind Nothing f = Nothing
  myBind (Just x) f = f x




