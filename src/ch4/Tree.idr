
data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)
%name Tree left, val, right

Eq elem => Eq (Tree elem) where
    (==) Empty Empty = ?Eq_rhs1_2
    (==) (Node left e right) (Node left' e' right') = left == left' && e == e' && right == right'
    (==) _ _ = False

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => orig
                                      GT => Node left val (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = let tree = listToTree xs in insert x tree

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ [val] ++ treeToList right
