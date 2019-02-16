import Data.List

data Tree a = Empty | Node a [Tree a]


--depth (Node a xs) ls| goal a ls= a
--                  | otherwise = False

next_elem f n = [f++[x] | x<-[2..n], x /= (last f)]



depth (f:fifo) ls visited | goal f ls= f
                        | (elem (last (visualize ls f)) visited == True) =  depth fifo ls visited
                        | otherwise = depth (fifo++(next_elem f (length ls))) ls ((last (visualize ls f)):visited)


goal a ls= checkList (last (visualize ls a))


initial n = [[x] | x<-[2..n]]

bfs ls = depth (initial (length ls)) ls []

checkList :: (Ord a) => [a] -> Bool
checkList [] = True
checkList [x] = True
checkList (x:y:xs) = x <= y && checkList (y:xs)

nreve n xs = (reverse (take n xs)) ++ (drop n xs)

visualize xs [] = [xs]
visualize xs (n:ns) = [xs] ++ (visualize (nreve n xs) ns)


flop n xs r | n == 1 = reverse r
            | ((max_index n xs) /= 1) = flop (n-1) (nreve n (nreve (max_index n xs) xs)) (n:((max_index n xs):r))
            | otherwise = flop (n-1) (nreve n xs) (n:r)

naive xs = flop (length xs) xs []

index n (x:xs) el | x==el = n
                  | otherwise = index (n+1) xs el

max_index n xs = index 1  xs (maximum (take n xs))
