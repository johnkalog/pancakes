import Data.List
import Data.Tuple

bidirectional ls = depth1 (initial (length ls)) (initial (length ls)) ls [(ls,[])] [((sort ls),[])]


initial n = [[x] | x<-[2..n]]

depth1 (f1:fifo1) fifo2 ls visited1 visited2 | (elem (last (visualize ls f1)) ( map (\x->fst x) visited2 )== True) = f1++(reverse (find_tuple (last (visualize ls f1)) visited2))
                        | (elem (last (visualize ls f1)) ( map (\x->fst x) visited1 ) == True) = depth2 fifo1 fifo2 ls visited1 visited2
                        | otherwise = depth2 (fifo1++(next_elem f1 (length ls))) fifo2 ls (visited1++[((last (visualize ls f1)),f1)]) visited2

depth2 fifo1 (f2:fifo2) ls visited1 visited2 | (elem (last (visualize (sort ls) f2)) ( map (\x->fst x) visited1 ) == True) = ((find_tuple (last (visualize (sort ls) f2)) visited1))++( reverse f2)
                        | (elem (last (visualize (sort ls) f2)) ( map (\x->fst x) visited2 ) == True) =  depth1 fifo1 fifo2 ls visited1 visited2
                        | otherwise = depth1 fifo1 (fifo2++(next_elem f2 (length ls))) ls visited1 (visited2++[((last (visualize (sort ls) f2)),f2)])

next_elem f n = [f++[x] | x<-[2..n], x /= (last f)]


nreve n xs = (reverse (take n xs)) ++ (drop n xs)

visualize xs [] = [xs]
visualize xs (n:ns) = [xs] ++ (visualize (nreve n xs) ns)

goal a ls= checkList (last (visualize ls a))



checkList :: (Ord a) => [a] -> Bool
checkList [] = True
checkList [x] = True
checkList (x:y:xs) = x <= y && checkList (y:xs)

find_tuple el (x:xs) | el==(fst x) = (snd x)
                     | otherwise = find_tuple el xs
