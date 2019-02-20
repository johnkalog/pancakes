import Data.List
import Data.Tuple

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



---- bidirectional ------

bidirectional ls = d1 (initial (length ls)) (initial (length ls)) 1 ls [(ls,[])] [((sort (zero_tup ls)),[])]


d1 (f1:fifo1) fifo2 dep ls visited1 visited2 | length f1 > dep = d2 (f1:fifo1) fifo2 dep  ls visited1 []
                                            | (length f1 == dep) && (elem (last (visualize ls f1)) ( map (\x->fst x) visited2 )== True) = f1++(reverse (find_tuple (last (visualize ls f1)) visited2))
                                            | otherwise = d1 (fifo1++( (next_elem f1 (length ls)))) fifo2 dep ls (((last (visualize ls f1)),f1):visited1) visited2

d2 fifo1 (f2:fifo2) dep ls visited1 visited2 | length f2 > dep = d1 fifo1 (f2:fifo2) (dep+1)  ls [] visited2
                                | (length f2 == dep) && (elem (last (visualize (sort (zero_tup ls)) f2)) ( map (\x->fst x) visited1 )== True) = ((find_tuple (last (visualize (sort (zero_tup ls)) f2)) visited1))++( reverse f2)
                                | otherwise = d2 fifo1 (fifo2++( (next_elem f2 (length ls)))) dep ls visited1 (((last (visualize (sort (zero_tup ls)) f2)),f2):visited2)

find_tuple el (x:xs) | el==(fst x) = (snd x)
                     | otherwise = find_tuple el xs

zero_tup [] = []
zero_tup ((x,y):xs) = (x,0):(zero_tup xs)
