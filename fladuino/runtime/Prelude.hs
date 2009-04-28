data Integer
data Float
data Char

data Maybe a = Nothing | Just a

data Either a = Left a | Right a

(==#) :: a -> a -> Bool
(/=#) :: a -> a -> Bool
(<#)  :: a -> a -> Bool
(<=#) :: a -> a -> Bool
(>=#) :: a -> a -> Bool
(>#)  :: a -> a -> Bool

(&&#)  :: Bool -> Bool -> Bool
(||#)  :: Bool -> Bool -> Bool

(+#)  :: a -> a -> a
(-#)  :: a -> a -> a
(*#)  :: a -> a -> a
(/#)  :: a -> a -> a

infixr 9  .
infixr 5  ++
infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 0  $

(==) = (==#)
(/=) = (/=#)
(<) = (<#)
(<=) = (<=#)
(>=) = (>=#)
(>) = (>#)

(&&) = (&&#)
(||) = (||#)

infixl 6 +
infixl 6 -
infixl 7 *
infixl 7 /

(+) = (+#)
(-) = (-#)
(*) = (*#)
(/) = (/#)

negate x = 0 - x

data Bool = True | False

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, x) = x

seq :: a -> b -> b
