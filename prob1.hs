-- Leonard Chan
-- 1. Compile using ghc
--    - $ ghc prob1.hs
-- 2. Run the executable produced
--    - ./prob1
-- Explanation:
-- Inserting a random permutation of integers into a BST produces a nearly optimal (balanced) BST because when navigating down the BST on insertion, there is always a 50/50 chance that the number inserted will be greater/less than the node currently being compared against. When inserting a number, the chance that that the relationship between the number and the current node and the relationship between this number and the next node are opposite increases.
-- For example, if the number to insert is greater than the head of the tree, there is a higher chance that this number is less than the next node to compare against. This is verified experminetally in this problem. In the above example output, the number of nodes in the BST is 6305 (6305 unique numbers are in the BST), and the height of this resulting BST is 29. For this BST to be an optimal BST, it must be close to balanced. The height of a balanced BST is log2(number of nodes). The height for a balanced BST cotnaining the same number of nodes is approximately 12.62. 29 (the actual height) is relatively close to 12 given over 6000 nodes, and both nombers are on the same magnitude.

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
