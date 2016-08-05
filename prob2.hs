-- Tree data type used to create the huffman tree
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

-- Check if heap contains element
heap_contains :: (Int, Char) -> [(Int, Char)] -> Bool
heap_contains (freq, c) [] = False
heap_contains (freq, c) ((head_freq, head_c):rest)
    | c == head_c = True
    | otherwise = heap_contains (freq, c) rest

-- Insert an element into a heap
insert_into_heap :: (Int, Char) -> [(Int, Char)] -> ((Int, Char) -> (Int, Char) -> Bool) -> [(Int, Char)]
insert_into_heap x [] _ _ = [x]
--insert_into_heap (x_freq, x_char) lst idk comp
--    | x_char == lst :: idx = lst
--    | otherwise = []

insert :: Int -> Tree Int -> Tree Int
insert x Empty = Branch x Empty Empty
insert x (Branch elem lb rb)
    | x == elem = Branch elem lb rb
    | x < elem = Branch elem (insert x lb) rb
    | x > elem = Branch elem lb (insert x rb)

main = print "ayy"
