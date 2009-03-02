v0 :: Float
v0 = let { v = 0.0 :: Float } in v
v1 :: (Float, Float) -> (Float, Float)
v1 = let {
        v = (\(x, prev) -> let { cur = 0.5 * x + 0.5 * prev } in (,) cur cur) ::
                ((Float, Float) -> (Float, Float))
        } in v
v2 :: Integer
v2 = let { v = 0 :: Integer } in v
f0 :: ((), Integer) -> (Float, Integer)
f0 = let { f (x, 0) = (,) 1.0 1; f (x, 1) = (,) 0.0 0 } in f
ssend3_out :: Float -> ()
ssend3_out x = ()
ssend3_in :: Float -> ()
sintegrate2_out :: Float -> ()
sintegrate2_out x = seq (ssend3_in x) ()
sintegrate2_in :: Float -> ()
sintegrate1_out :: Float -> ()
sintegrate1_out x = seq (sintegrate2_in x) ()
sintegrate1_in :: () -> ()
clock0_out :: () -> ()
clock0_out x = seq (sintegrate1_in x) ()
clock0_in :: () -> ()
clock0_in x = clock0_out ()