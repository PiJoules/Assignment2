import System.Random

-- Declare Tree data type to contain an Empty data type
-- and a Branch containing some data type, and the left and
-- right sub trees
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

-- Insertion into an empty tree
-- If tree is empty, create and return a leaf
-- Otherwise, compare against head of tree.
-- If greater than, go to left subtree.
-- If less than, go to right.
-- If Equal, return original tree.
insert :: Int -> Tree Int -> Tree Int
insert x Empty = Branch x Empty Empty
insert x (Branch elem lb rb)
    | x == elem = Branch elem lb rb
    | x < elem = Branch elem (insert x lb) rb
    | x > elem = Branch elem lb (insert x rb)

tree_from_list :: [Int] -> Tree Int -> Tree Int
tree_from_list [] tree = tree
tree_from_list (x:xs) tree = insert x (tree_from_list xs tree)

height :: Tree Int -> Int
height Empty = 0
height (Branch elem lb rb) = let
    lb_height = height lb
    rb_height = height rb
    in
    if lb_height > rb_height then
        lb_height + 1
    else
        rb_height + 1

count :: Tree Int -> Int
count Empty = 0
count (Branch elem lb rb) = (count lb) + (count rb) + 1

main = do
    gen <- newStdGen
    let n = 10000
    let nums = take n $ randomRs (0, n) gen :: [Int]
    let tree = tree_from_list nums Empty
    let num_nodes = count tree
    print $ num_nodes
    print $ height tree
    print $ logBase 2.0 (fromIntegral num_nodes)
