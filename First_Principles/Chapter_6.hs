module Chapter_6 where

data Trivial = Trivial

instance Eq Trivial where
    Trivial == Trivial = True

data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
data Date = Date WeekDay Int


instance Eq WeekDay where
    Mon == Mon = True
    Tue == Tue = True
    Wed == Wed = True
    Thu == Thu = True
    Fri == Fri = True
    Sat == Sat = True
    Sun == Sun = True
    _   == _   = False

instance Eq Date where
    (Date w d) == (Date w' d') = (w == w') && (d == d')


data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
    (TisAn i) == (TisAn j) = i == j

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
    (Two a b) == (Two c d) = (a==c) && (b==d)

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
    (TisAnInt a) == (TisAnInt b) = a == b
    (TisAString s) == (TisAString t) = s == t
    _ == _ = False

data Pair a = Pair a a
instance (Eq a) => Eq (Pair a) where
    (Pair a b) == (Pair c d) = (a == c) && (b == d)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b
