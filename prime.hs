divides n d = d `rem` n == 0

ldf k n
  | divides k n = k
  | k ^ 2 > n = n
  | otherwise = ldf (k + 1) n

ld = ldf 2

prime0 n
  | n < 1 = error "Not a real number"
  | n == 1 = False
  | otherwise = ld n == n