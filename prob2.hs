import Data.Map
import Data.List

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf x = Branch x Empty Empty

-- Check if heap contains element
heap_contains :: Maybe Char -> [(Maybe Char, Int)] -> Bool
heap_contains c [] = False
heap_contains Nothing _ = False
heap_contains c ((Nothing, head_freq):rest) = heap_contains c rest
heap_contains (Just c) (((Just head_c), head_freq):rest) =
    if c == head_c then
        True
    else
        heap_contains (Just c) rest

-- Swap elements in list
swap :: Int -> Int -> [a] -> [a]
swap i j xs = let
    elemI = xs !! i
    elemJ = xs !! j
    left = take i xs
    middle = take (j - i - 1) (drop (i + 1) xs)
    right = drop (j + 1) xs
    in
    left ++ [elemJ] ++ middle ++ [elemI] ++ right

-- Reformat heap after appending
reformat :: [(a, Int)] -> Int -> [(a, Int)]
reformat [x] _ = [x]  -- list of size 1
reformat lst 0 = lst  -- reached the head
reformat lst idx = let
    parent_idx = (idx - 1) `div` 2
    (_, freq) = lst !! idx
    (_, parent_freq) = lst !! parent_idx
    in
    if freq < parent_freq then
        reformat (swap parent_idx idx lst) parent_idx
    else
        lst

-- Insert an element into a heap
insert_into_heap :: (Maybe Char, Int) -> [(Maybe Char, Int)] -> [(Maybe Char, Int)]
insert_into_heap x [] = [x]
insert_into_heap (c, freq) lst
    | heap_contains c lst = lst
    | otherwise = reformat (lst ++ [(c, freq)]) (length lst)

-- Create list of tuples from string
heapify_string :: [Char] -> [(Maybe Char, Int)]
heapify_string str = let
    lst = toList $ fromListWith (+) [(Just c, 1) | c <- str]
    in
    reformat lst ((length lst) - 1)

-- Convert list of frequencies to list of trees of frequencies
heap_of_trees :: [(Maybe Char, Int)] -> [Tree (Maybe Char, Int)]
heap_of_trees [] = []
heap_of_trees (x:xs) = (leaf x):(heap_of_trees xs)

-- Sort a list of trees
tree_compare (Branch (_, freq) _ _) (Branch (_, freq2) _ _)
    | freq < freq2 = LT
    | otherwise = GT

-- Function for actually creating the huffman tree
huffman :: [Tree (Maybe Char, Int)] -> Tree (Maybe Char, Int)
huffman [x] = x
huffman ((Branch (c, freq) lb rb):(Branch (c2, freq2) lb2 rb2):rest) = let
    lt = Branch (c, freq) lb rb
    rt = Branch (c2, freq2) lb2 rb2
    combined = Branch (Nothing, freq + freq2) lt rt
    lst = rest ++ [combined]
    in
    huffman $ sortBy tree_compare lst

left_branch Empty = Empty
left_branch (Branch _ lb _) = lb

right_branch Empty = Empty
right_branch (Branch _ _ rb) = rb


main = do
    let test_string = "111112222222333333333366666666666666666666666666666666666666666666655555555555555555555444444444444444"
    let test_elems = heapify_string test_string
    let result = huffman $ heap_of_trees $ test_elems
    print test_elems
    print result
