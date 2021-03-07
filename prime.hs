divides n d = d `rem` n == 0

ldf k n | divides k n = k
        | k ^ 2 > n   = n
        | otherwise   = ldf (k + 1) n

ld = ldf 2

prime0 n | n < 1     = error "Not a real number"
         | n == 1    = False
         | otherwise = ld n == n

primes0 = filter prime0 [2 ..]

ldp n = ldpf primes1 n

ldpf (p : ps) n | divides p n = p
                | p ^ 2 > n   = n
                | otherwise   = ldpf ps n

primes1 = 2 : filter prime [3 ..]

prime n | n < 1     = error "not a positive integer"
        | n == 1    = False
        | otherwise = ldp n == n
