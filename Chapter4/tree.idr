

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)
%name Tree tree, tree1, tree2


insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node tleft val tright) = case compare x val of
                                        LT => Node (insert x tleft) val tright
                                        EQ => orig
                                        GT => Node tleft val (insert x tright)
