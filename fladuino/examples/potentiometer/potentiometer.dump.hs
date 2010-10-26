v0 :: Integer -> Integer
v0 = let { v = (\tc:107 -> (/) tc:107 4) :: (Integer -> Integer) } in v
toggle1_out :: () -> ()
toggle1_out x = ()
toggle1_in :: Integer -> ()
idle0_out :: Integer -> ()
idle0_out x = seq (toggle1_in x) ()
idle0_in :: Integer -> Integer
idle0_in x = idle0_out x
setValue5_out :: () -> ()
setValue5_out x = ()
setValue5_in :: Integer -> ()
smap4_out :: Integer -> ()
smap4_out x = seq (setValue5_in x) ()
smap4_in :: Integer -> ()
smap4_in x = smap4_out (v0 x)
valueOf3_out :: Integer -> ()
valueOf3_out x = seq (smap4_in x) ()
valueOf3_in :: Integer -> ()
idle2_out :: Integer -> ()
idle2_out x = seq (valueOf3_in x) ()
idle2_in :: Integer -> Integer
idle2_in x = idle2_out x