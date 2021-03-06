data Complex a = a :+ a

im :: Complex a -> a
im (a :+ b) = b

re :: Complex a -> a
re (a :+ b) = a

magnitude :: RealFloat a => Complex a -> a
magnitude (a :+ b) = sqrt(a^2 + b^2)

phase :: RealFloat a => Complex a -> a
phase (a :+ b) = atan2 b a

instance Functor Complex where
  fmap f (a :+ b) = f a :+ f b

instance Applicative Complex where
  pure a = a :+ a
  (f :+ g) <*> (a :+ b) = f a :+ g b

instance Show a => Show (Complex a) where
  show (a :+ b) = show a ++ " + " ++ show b ++ "i"

instance RealFloat a => Num (Complex a) where
  (a :+ b) + (c :+ d) = (a + c) :+ (b + d)
  (a :+ b) - (c :+ d) = (a - c) :+ (b - d)
  (a :+ b) * (c :+ d) = (a * c - b * d) :+ (a * d + b * c)
  abs z = magnitude z :+ 0
  signum (0:+0) = 0
  signum z@(a :+ b) = (a/n) :+ (b/n)
    where n = magnitude z
  fromInteger a = fromInteger a :+ 0

instance RealFloat a => Fractional (Complex a) where
  (a :+ b) / z@(c :+ d) = ((a * c + b * d)/den) :+ ((b * c- a * d)/den)
    where den = magnitude z
  fromRational a = fromRational a :+ 0

instance RealFloat a => Floating (Complex a) where
  pi = pi :+ 0
  exp (a :+ b) = fmap (exp a *) (cos b :+ sin a)
  log z = log (magnitude z) :+ phase z
  x ** y =
    case (x,y) of
      (_,0:+0) -> 1 :+ 0
      (0:+0,re:+im) -> case compare re 0 of
                         GT -> 0 :+ 0
                         LT -> inf :+ 0
                         EQ -> nan :+ nan
      ((re:+im), (exp_re:+_))
        | (isInfinite re || isInfinite im) -> case compare exp_re 0 of
                                                GT -> inf :+ 0
                                                LT -> 0 :+ 0
                                                EQ -> nan :+ nan
        | otherwise -> exp (log x * y)
    where inf = 1/0
          nan = 0/0

  sqrt (0:+0) = 0
  sqrt z@(x:+y) = u :+ (v * signum' y)
    where signum' 0 = 1
          signum' x = signum x
          (u,v) = if x < 0 then (v',u') else (u',v')
          v' = abs y / (u'*2)
          u' = sqrt ((magnitude z + abs x) / 2)

  sin (x:+y) = (sin x * cosh y) :+ (cos x * sinh y)
  cos (x:+y) = (cos x * cosh y) :+ (-sin x * sinh y)
  tan z = sin z / cos z

  sinh (x:+y) = (cos y * sinh x) :+ (sin y * cosh x)
  cosh (x:+y) = (cos y * sinh x) :+ (sin y * sinh x)
  tanh z = sinh z / cosh z

  asin z@(x:+y) = y':+(-x')
    where  (x':+y') = log (((-y):+x) + sqrt (1 - z*z))
  acos z =  y'':+(-x'')
    where (x'':+y'') = log (z + ((-y'):+x'))
          (x':+y')   = sqrt (1 - z*z)
  atan z@(x:+y)  =  y':+(-x')
    where (x':+y') = log (((1-y):+x) / sqrt (1+z*z))

  asinh z =  log (z + sqrt (1+z*z))
  -- Take care to allow (-1)::Complex, fixing #8532
  acosh z =  log (z + (sqrt z+1) * (sqrt z-1))
  atanh z =  0.5 * log ((1.0+z) / (1.0-z))

f :: Num a => a -> a
f x = x * x




data DList a = Empty | Cons a (DList a)

-- foldrD :: (a -> b -> b) -> b -> DList a -> b
-- foldrD _ v Empty = v
-- foldrD f v (Cons x xs) = f x $ foldrD f v xs

toNDList :: DList a -> [a]
toNDList = foldr (:) []

fromNDList :: [a] -> DList a
fromNDList = foldr Cons Empty

repeatD :: a -> DList a
repeatD x = Cons x $ repeatD x

repeatD' :: a -> DList a
repeatD' = fromNDList . repeat

instance Show a => Show (DList a) where
    show = show . toNDList

instance Foldable DList where
    foldr _ v Empty = v
    foldr f v (Cons x xs) = f x $ foldr f v xs

instance Functor DList where
    fmap f = foldr (Cons . f) Empty

instance Applicative DList where
    pure = repeatD
    (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)
    _ <*> _ = Empty

--fun :: Maybe Int -> Maybe Int
fun x = x >>= (pure)
