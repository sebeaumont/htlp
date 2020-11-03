module Numbers where

-- | The nth factorial  
fac :: Integer -> Integer
fac = let f a 0 = a
          f a n = f (n*a) (n-1)
       in f 1

-- | Compute nth fibonacci number.
fib :: Integer -> Integer
fib = let f a1 a2 i n
            | i == n = a
            | otherwise = f a a1 (i+1) n
            where a = a1 + a2
      in f 0 1 1 

-- | The fibonacci numbers.
fibonacci :: [Integer]
fibonacci = [fib n | n <- [1..]] 

phi :: Integer -> Rational
phi n = fibo n / fibo (n+1)
  where
    fibo = toRational . fib

