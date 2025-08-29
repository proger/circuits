module Le.MLP1 where

axpy _ [] y = y
axpy [] _ y = y
axpy (a:as) (x:xs) y = a * x + axpy as xs y

gemv _ [] _ = []
gemv as (x:xs) y = axpy as x y : gemv as xs y

gemm [] _ = []
gemm (a:as) bs = gemv a bs 0 : gemm as bs

relu [] = []
relu (x:xs) = max 0 x : relu xs

fexp [] = []
fexp (x:xs) = exp x : fexp xs

smaxf xs = map (/z) n
  where
    n = map exp xs
    z = sum n

sadd (ml,dl) (mr,dr) = (m, dl * exp (ml - m) + dr * exp (mr - m))
  where m = max ml mr

smaxo xs = go xs m d
  where
    (m, d) = norm (-1/0, 0) xs

    norm (m, d) [] = (m, d)
    norm (m, d) (x:xs) = sadd (x, 1) (norm (m, d) xs)

    go [] _ _ = []
    go (x:xs) m d = exp (x - m) / d : go xs m d    

fmlp w1 w2 x = smaxo y
  where h  = gemv x w1 0
        h' = relu h
        y  = gemv h' w2 0

main = do
  print (fmlp [[1, 1], [1, 1], [1, 1]] [[1, 2, 3], [4, 5, 6]] [1, 1])
