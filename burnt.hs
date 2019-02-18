flop n xs r | (n == 1 && (snd (head xs)) == 1)= reverse (1:r)
            | (n == 1 && (snd (head xs)) == 0)= reverse r
            | ((max_index n xs) /= 1 && (snd (nth (max_index n xs) xs) == 0)) = flop (n-1) (nreve n (nreve (max_index n xs) xs)) (n:((max_index n xs):r))
            | ((max_index n xs) /= 1 && (snd (nth (max_index n xs) xs) == 1)) = flop (n-1) (nreve n (nreve 1 (nreve (max_index n xs) xs))) (n:(1:((max_index n xs):r)))
            | ((max_index n xs) == 1 && (snd (head xs)) == 0) = flop (n-1) (nreve n (nreve 1 xs)) (n:(1:r))
            | otherwise = flop (n-1) (nreve n xs) (n:r)

naive xs = flop (length xs) xs []


max_index n xs = index 1  xs (maximum (take n xs))

nreve n xs = (change (reverse (take n xs))) ++ (drop n xs)

change [] = []
change ((x,1):xs) = ((x,0):(change xs))
change ((x,0):xs) = ((x,1):(change xs))


visualize xs [] = [xs]
visualize xs (n:ns) = [xs] ++ (visualize (nreve n xs) ns)


index n (x:xs) el | x==el = n
                  | otherwise = index (n+1) xs el


nth n xs = last (take n xs)



depth (f:fifo) ls visited | goal f ls= f
--                        | (elem (last (visualize ls f)) visited == True) =  depth fifo ls visited
                        | otherwise = depth (fifo++(next_elem f (length ls))) ls ((last (visualize ls f)):visited)


goal a ls= checkList (last (visualize ls a)) && check_burnt (last (visualize ls a))

check_burnt [] = True
check_burnt ((x,y):ls) | y == 0 = check_burnt ls
                       | otherwise = False

bfs ls = depth (initial (length ls)) ls []




checkList :: (Ord a) => [a] -> Bool
checkList [] = True
checkList [x] = True
checkList (x:y:xs) = x <= y && checkList (y:xs)


initial n = [[x] | x<-[1..n]]

next_elem f n = [f++[x] | x<-[1..n], x /= (last f)]
