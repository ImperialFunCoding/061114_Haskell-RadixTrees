module RadixTrees where
data IntTree = EmptyTree | InternalNode IntTree Int IntTree
               deriving Show

buildIntTree = foldr add EmptyTree

add x EmptyTree = InternalNode EmptyTree x EmptyTree
add x (InternalNode l y r) 
  = if x == y 
    then (InternalNode l y r)
    else if x <= y then InternalNode (add x l) y r
                   else InternalNode l y (add x r)

a, m :: Integer
m = 1073741824
a = 16387

rand s 
  = fromInteger s / fromInteger m : rand s' where s' = (s * a) `mod` m
          
randomInts :: Int -> Int -> Integer -> [Int]
randomInts m n s 
  = take m (map (round . (+1) . (* (fromIntegral n))) (rand s))

rs :: [Int]
rs = randomInts 1000 1000 816211275

rs1 :: [Int]
rs1 = randomInts 20 1000 816211275

--------------------------------------------------------------------------

data Colour = Black | White
              deriving (Eq, Ord, Show)

data RadixTree = Leaf Colour | Node Colour RadixTree RadixTree 
                 deriving (Eq, Ord, Show)

type BitString = [Int]


-----------------------------------------------------------------------------
-- Some test trees...

figure :: RadixTree
figure = Node Black (Leaf White)
                    (Node White (Leaf Black)
                                 (Node White (Node Black (Leaf White)
                                                           (Leaf Black))
                                              (Leaf White)))

t1 :: IntTree
t1 = InternalNode (InternalNode EmptyTree 
                                 8 
                                 (InternalNode EmptyTree 12 EmptyTree))
                  20
                  EmptyTree

t2 :: RadixTree
t2 = Node Black (Node Black (Leaf White) 
                             (Node White (Leaf Black) (Leaf White))) 
                (Leaf White)

--------------------------------------------------------------------------

size :: IntTree -> Int
size (EmptyTree)          = 1
size (InternalNode l x r) = 13 + size l + size r

size' :: RadixTree -> Int 
size' (Leaf White) = 1
size' (Leaf Black) = 0
size' (Node White l r) = 9 + size' l + size' r
size' (Node Black l r) = 0 + size' l + size' r


binary :: Int -> BitString
binary n = (reverse . bin) n
 where
   bin n
     | n < 2     = [n]
     | otherwise = (mod n 2) : (bin (div n 2))



insert :: BitString -> RadixTree -> RadixTree
insert []     (Leaf _)     = Leaf White
insert []     (Node _ l r) = Node White l r
insert (0:ns) (Node c l r) = Node c (insert ns l) r
insert (1:ns) (Node c l r) = Node c l (insert ns r)
insert ns     (Leaf c)     = insert ns (Node c (Leaf Black) (Leaf Black))




buildRadixTree :: [Int] -> RadixTree
buildRadixTree ns = foldl (flip insert) (Leaf Black) (map binary ns)

{- Needs Work
buildList :: RadixTree -> [BitString]
buildList (Node c (Node c l r) (Node c' l' r'))
    | c == White = 0:(buildList l++buildList r)
-}

member :: Int -> RadixTree -> Bool
member n t = (insert (binary n) t) == t

ui :: (Bool->Bool->Bool)->RadixTree -> RadixTree -> RadixTree
ui f (Node c l r) (Node c' l' r')
    = if (f (c == White) (c' == White))
      then Node White (ui f l l') (ui f r r') 
      else Node Black (ui f l l') (ui f r r') 
ui f (Node c l r) (Leaf c') 
    = if (f (c == White) (c' == White))
      then Node White l r
      else Node Black l r
ui f (Leaf c') (Node c l r) 
    = if (f (c == White) (c' == White))
      then Node White l r
      else Node Black l r
ui f (Leaf c) (Leaf c')  
    = if (f (c == White) (c' == White))
      then Leaf White
      else Leaf Black


union, intersection :: RadixTree -> RadixTree -> RadixTree
union t t' = ui (||) t t'
intersection t t' = ui (&&) t t'

{-
union (Node c l r) (Node c' l' r')
    = if (c == White || c' == White)
      then Node White (union l l') (union r r') 
      else Node Black (union l l') (union r r') 
union (Node c l r) (Leaf c') 
    = if (c == White || c' == White)
      then Node White l r
      else Node Black l r
union (Leaf c') (Node c l r) 
    = if (c == White || c' == White)
      then Node White l r
      else Node Black l r
union (Leaf c) (Leaf c')  
    = if (c == White || c' == White)
      then Leaf White
      else Leaf Black

intersection (Node c l r) (Node c' l' r')
    = if (c == White && c' == White)
      then Node White (intersection l l') (intersection r r') 
      else Node Black (intersection l l') (intersection r r') 
intersection (Node c l r) (Leaf c') 
    = if (c == White && c' == White)
      then Node White l r
      else Node Black l r
intersection (Leaf c') (Node c l r) 
    = if (c == White && c' == White)
      then Node White l r
      else Node Black l r
intersection (Leaf c) (Leaf c')  
    = if (c == White && c' == White)
      then Leaf White
      else Leaf Black
-}




