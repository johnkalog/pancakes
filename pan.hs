import Data.List
import Data.Tuple

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

-- positions current [] = []
-- positions current (x:xs) | (snd x)==0 && (fst x)==min_zero (x:xs) = [(current,1)]++(positions (current+1) xs)
--                          | otherwise = positions current xs
--
-- low_ls ls = positions 1 (tup ls)
--
-- tup ls = map (\x->(x,0)) ls
--
-- delete_one [] = []
-- delete_one (x:xs) | (snd x)==0 = [x]++( delete_one xs)
--                   | otherwise = delete_one xs
--
-- min_zero ls = minimum (map (\x-> fst x) (delete_one ls))

how_many el ls = sum (many_lower el ls)

many_lower _ [] = []
many_lower el (x:xs) | x<el = [1]++(many_lower el xs)
                     | otherwise = [0]++(many_lower el xs)

positions (x:xs) = map (\y->how_many y (x:xs) ) (x:xs)

change_nth n el (x:xs) | n==1 = el:xs
                       | otherwise = x:(change_nth (n-1) el xs)

removeItem _ [] = []
removeItem x (y:ys) | x==y = removeItem x ys
                    | otherwise = y:removeItem x ys

-- depth_stack (f:fifo) ls visited [] list result = result
depth_stack (f:fifo) ls visited lists list result | lists==[] = result
                        | (elem (last (visualize ls f)) visited == True) =  depth_stack fifo ls visited lists list result
                        | (goal_lists f ls lists 1)/=([],0) = depth_stack (fifo++(next_elem f (length ls))) ls ((last (visualize ls f)):visited)
                                                                    ( removeItem (fst (goal_lists f ls lists 1)) lists)
                                                                    list
                                                                    (result++[((reverse f),(snd (goal_lists f ls list 1)))])
--                                                                    (change_nth (snd (goal_lists f ls ( removeItem (fst (goal_lists f ls lists 1)) lists) 1)) (reverse f) result)

                        | otherwise = depth_stack (fifo++(next_elem f (length ls))) ls ((last (visualize ls f)):visited) lists list result

initial_zero n = [x | x<-[0..n-1]]


batch (x:xs) = depth_stack (initial (length x)) (initial_zero (length x)) [] (map (\x->positions x) (x:xs)) (map (\x->positions x) (x:xs)) [([],0)]

goal_lists a ls [] n = ([],0)
goal_lists a ls (x:xs) n | goal_stack a ls x = (x,n)
                         | otherwise = goal_lists a ls xs (n+1)

goal_stack a ls for_check | (last (visualize ls a))==for_check = True
                          | otherwise = False
