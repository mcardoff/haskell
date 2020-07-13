newtype Sum a = Sum {getSum :: a} deriving (Eq, Ord)

instance (Show a) => Show (Sum a) where
  show (Sum a) = "Sum " ++ show a
  
instance Functor Sum  where
  fmap f (Sum a) = Sum $ f a

instance Applicative Sum where
  pure = Sum
  (Sum f) <*> (Sum x) = Sum $ f x

instance Monad Sum where
  (Sum a) >>= f = f a
