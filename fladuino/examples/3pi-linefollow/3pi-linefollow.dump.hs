compute0 :: (Float, Float) -> (Integer, Integer)
compute0 = let {
        compute (v, omega) = let {
                motorLvel = v * if omega <= 0.0 then 1.0 else 1.0 - omega;
                motorRvel = v * if omega >= 0.0 then 1.0 else 1.0 + omega
                } in (,) (round (motorLvel * 255.0)) (round (motorRvel * 255.0))
        } in compute
f0 :: Integer -> (Float, Float)
f0 = let {
        f linepos = if linepos < 1000 then (,) (0.0 - 1.0) 0.5 else if linepos >
            3000 then (,) 0.0 0.5 else (,) 1.0 0.0
        } in f
motors4_out :: () -> ()
motors4_out x = ()
motors4_in :: (Integer, Integer) -> ()
smap3_out :: (Integer, Integer) -> ()
smap3_out x = seq (motors4_in x) ()
smap3_in :: (Float, Float) -> ()
smap3_in x = smap3_out (compute0 x)
smap2_out :: (Float, Float) -> ()
smap2_out x = seq (smap3_in x) ()
smap2_in :: Integer -> ()
smap2_in x = smap2_out (f0 x)
valueOf1_out :: Integer -> ()
valueOf1_out x = seq (smap2_in x) ()
valueOf1_in :: Integer -> ()
idle0_out :: Integer -> ()
idle0_out x = seq (valueOf1_in x) ()
idle0_in :: Integer -> ()
idle0_in x = idle0_out x