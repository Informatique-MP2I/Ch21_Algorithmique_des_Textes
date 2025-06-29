type huffman_tree =
  | Leaf of int * int
  | Node of int * huffman_tree * huffman_tree  
