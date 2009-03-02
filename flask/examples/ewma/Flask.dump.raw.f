(==#) :: \/a . a -> a -> Bool;
(/=#) :: \/a . a -> a -> Bool;
(<#) :: \/a . a -> a -> Bool;
(<=#) :: \/a . a -> a -> Bool;
(>=#) :: \/a . a -> a -> Bool;
(>#) :: \/a . a -> a -> Bool;
(&&#) :: Bool -> Bool -> Bool;
(||#) :: Bool -> Bool -> Bool;
(+#) :: \/a . a -> a -> a;
(-#) :: \/a . a -> a -> a;
(*#) :: \/a . a -> a -> a;
(/#) :: \/a . a -> a -> a;
seq :: \/a, b . a -> b -> b;
data Bool :: * where {
    True :: Bool;
    False :: Bool
}
data Either :: * -> * where {
    Left :: \/a . a -> Either a;
    Right :: \/a . a -> Either a
}
data Maybe :: * -> * where {
    Nothing :: \/a . Maybe a;
    Just :: \/a . a -> Maybe a
}
data Char :: *
data Float :: *
data Integer :: *
let {
    snd :: \/a, b . (a, b) -> b = /\a . /\b . \tc:4 :: (a,
    b) . case tc:4 of tc:41 {
        (,) (tc:59 :: a) (x :: b) -> x;
        (_ :: (a, b)) -> error [b] "No match"
    }
}
let {
    fst :: \/a, b . (a, b) -> a = /\a . /\b . \tc:13 :: (a,
    b) . case tc:13 of tc:131 {
        (,) (x :: a) (tc:62 :: b) -> x;
        (_ :: (a, b)) -> error [a] "No match"
    }
}
let {
    (/) :: \/a . a -> a -> a = /\a . (/#) [a]
}
let {
    (*) :: \/a . a -> a -> a = /\a . (*#) [a]
}
let {
    (-) :: \/a . a -> a -> a = /\a . (-#) [a]
}
let {
    negate :: Integer -> Integer = \x :: Integer . (-) [Integer] 0 x
}
let {
    (+) :: \/a . a -> a -> a = /\a . (+#) [a]
}
let {
    (||) :: Bool -> Bool -> Bool = (||#)
}
let {
    (&&) :: Bool -> Bool -> Bool = (&&#)
}
let {
    (>) :: \/a . a -> a -> Bool = /\a . (>#) [a]
}
let {
    (>=) :: \/a . a -> a -> Bool = /\a . (>=#) [a]
}
let {
    (<=) :: \/a . a -> a -> Bool = /\a . (<=#) [a]
}
let {
    (<) :: \/a . a -> a -> Bool = /\a . (<#) [a]
}
let {
    (/=) :: \/a . a -> a -> Bool = /\a . (/=#) [a]
}
let {
    (==) :: \/a . a -> a -> Bool = /\a . (==#) [a]
}
ssend3_in :: Float -> ();
sintegrate2_in :: Float -> ();
sintegrate1_in :: () -> ();
let {
    clock0_out :: () -> () = \x :: () . seq [()] [()] (sintegrate1_in x) ()
}
let {
    clock0_in :: () -> () = \x :: () . clock0_out ()
}
let {
    sintegrate1_out :: Float -> () = \x :: Float . seq [()] [()] (sintegrate2_in
    x) ()
}
let {
    sintegrate2_out :: Float -> () = \x :: Float . seq [()] [()] (ssend3_in x)
    ()
}
let {
    ssend3_out :: Float -> () = \x :: Float . ()
}
let {
    f0 :: ((), Integer) -> (Float, Integer) = let {
        f :: \/a . (a, Integer) -> (Float, Integer) = /\a . \tc:77 :: (a,
        Integer) . case tc:77 of tc:771 {
            (,) (x :: a) (tc:104 :: Integer) -> case tc:104 of tc:1041 {
                    0 -> (,) [Float] [Integer] 1.0 1;
                    1 -> (,) [Float] [Integer] 0.0 0;
                    (_ :: Integer) -> error [(Float, Integer)] "No match"
                };
            (_ :: (a, Integer)) -> error [(Float, Integer)] "No match"
        }
    } in f [()]
}
let {
    v2 :: Integer = let {
        v :: Integer = 0
    } in v
}
let {
    v1 :: (Float, Float) -> (Float, Float) = let {
        v :: (Float, Float) -> (Float, Float) = \tc:100 :: (Float,
        Float) . case tc:100 of tc:1001 {
            (,) (x :: Float) (prev :: Float) -> let {
                    cur :: Float = (+) [Float] ((*) [Float] 0.5 x) ((*) [Float]
                    0.5 prev)
                } in (,) [Float] [Float] cur cur;
            (_ :: (Float, Float)) -> error [(Float, Float)] "No match"
        }
    } in v
}
let {
    v0 :: Float = let {
        v :: Float = 0.0
    } in v
}