Leonard Chan

lc599@drexel.edu

Problem 1 Example Output
lc599@tux64-14:~/CS360/pa2$ ghc prob1.hs
[1 of 1] Compiling Main             ( prob1.hs, prob1.o )
Linking prob1 ...
lc599@tux64-14:~/CS360/pa2$ ./prob1
6305
29
12.622280655215583

Explanation:
Inserting a random permutation of integers into a BST produces a nearly optimal (balanced) BST because when navigating down the BST on insertion, there is always a 50/50 chance that the number inserted will be greater/less than the node currently being compared against. When inserting a number, the chance that that the relationship between the number and the current node and the relationship between this number and the next node are opposite increases.
For example, if the number to insert is greater than the head of the tree, there is a higher chance that this number is less than the next node to compare against. This is verified experminetally in this problem. In the above example output, the number of nodes in the BST is 6305 (6305 unique numbers are in the BST), and the height of this resulting BST is 29. For this BST to be an optimal BST, it must be close to balanced. The height of a balanced BST is log2(number of nodes). The height for a balanced BST cotnaining the same number of nodes is approximately 12.62. 29 (the actual height) is relatively close to 12 given over 6000 nodes, and both nombers are on the same magnitude.


Problem 2 Example output
lc599@tux64-14:~/CS360/pa2$ ghc prob2.hs
[1 of 1] Compiling Main             ( prob2.hs, prob2.o )
Linking prob2 ...
lc599@tux64-14:~/CS360/pa2$ ./prob2
"Test string:"
"aaaaaaaabbbcdefgh"
"Frequencies:"
[(Just 'a',8),(Just 'b',3),(Just 'c',1),(Just 'd',1),(Just 'e',1),(Just 'f',1),(Just 'g',1),(Just 'h',1)]
"Huffman tree:"
Branch (Nothing,17) (Branch (Just 'a',8) Empty Empty) (Branch (Nothing,9) (Branch (Nothing,4) (Branch (Nothing,2) (Branch (Just 'c',1) Empty Empty) (Branch (Just 'd',1) Empty Empty)) (Branch (Nothing,2) (Branch (Just 'e',1) Empty Empty) (Branch (Just 'f',1) Empty Empty))) (Branch (Nothing,5) (Branch (Nothing,2) (Branch (Just 'g',1) Empty Empty) (Branch (Just 'h',1) Empty Empty)) (Branch (Just 'b',3) Empty Empty)))
"Encoding:"
"00000000111111111100010011010101111001101"
"Decoding the encoding:"
"aaaaaaaabbbcdefg"


Problem 3 Example Output
lc599@tux64-14:~/CS360/pa2$ mit-scheme
MIT/GNU Scheme running under GNU/Linux
Type `^C' (control-C) followed by `H' to obtain information about interrupts.

Copyright (C) 2014 Massachusetts Institute of Technology
This is free software; see the source for copying conditions. There is NO warranty;
not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Image saved on Saturday May 17, 2014 at 2:39:25 AM
  Release 9.2 || Microcode 15.3 || Runtime 15.7 || SF 4.41 || LIAR/x86-64 4.118
  Edwin 3.116

1 ]=> (load "prob3.scm")

;Loading "prob3.scm"... done
;Value: define-variable!

1 ]=>

