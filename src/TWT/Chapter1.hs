module TWT.Chapter1 where

exercise_1_4_1_ia ::  (b -> c -> a) -> (b, c) -> a
exercise_1_4_1_ia = uncurry

exercise_1_4_1_ib :: ((b, c) -> a) -> b -> c -> a
exercise_1_4_1_ib = curry

-- | a^b x a^c -> a^(b+c)
exercise_1_4_ii :: (b -> a) -> (c -> a) -> Either b c -> a
exercise_1_4_ii f _ (Left x) = f x
exercise_1_4_ii _ g (Right x) = g x

-- | a^(b+c) -> a^b x a^c  
exercise_1_4_ii' :: (Either b c -> a) -> (b -> a, c -> a)
exercise_1_4_ii' f = (f . Left, f . Right)

-- | (a b)^c == a^c x b^c
exercise_1_4_iii :: (c -> (a, b)) -> (c -> a, c -> b)
exercise_1_4_iii f = (fst . f, snd . f)

-- | a^c x b^c == (a b)^c
exercise_1_4_iii' :: (c -> a) -> (c -> b) -> c -> (a, b)
exercise_1_4_iii' f g x = (f x, g x) 
