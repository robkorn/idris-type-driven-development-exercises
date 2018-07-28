
module Tree

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Functor Tree where
  map func Empty = Empty
  map func (Node l el r) = Node (map func l) (func el) (map func r)
