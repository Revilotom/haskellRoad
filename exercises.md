# 1.4

If we replaced > with >= then nothing will change.
That is because the case where the two values are equal is already handled by the guard above it.

# 1.6

Integral -> Integral -> Integral

# 1.9

maxList :: [Int] -> Int
maxList [x ] = x
maxList (x : xs) = max x \$ maxList xs

# 1.10

rmFst :: [Int] -> Int -> [Int]
rmFst [x ] n = if x == n then [] else [x]
rmFst (x : xs) n = if x == n then xs else (x : rmFst xs n)

# 1.10

count :: Char -> String -> Int
count \_ [] = 0
count c (x : xs) | c == x = 1 + count c xs
| otherwise = count c xs
