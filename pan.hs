

nreve n xs = (reverse (take n xs)) ++ (drop n xs)

visualize xs [] = [xs]
visualize xs (n:ns) = [xs] ++ (visualize (nreve n xs) ns)

--flop 0 xs r = reverse r

flop n xs r | n == 1 = reverse r
            | ((max_index n xs) /= 1) = flop (n-1) (nreve n (nreve (max_index n xs) xs)) (n:((max_index n xs):r))
            | otherwise = flop (n-1) (nreve n xs) (n:r)

naive xs = flop (length xs) xs []

index n (x:xs) el | x==el = n
                  | otherwise = index (n+1) xs el

max_index n xs = index 1  xs (maximum (take n xs))
