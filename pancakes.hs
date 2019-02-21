import Data.List
import Data.Tuple

----------------------------------------------------------------naive--------------------------------------------------------------------------------

max_index n xs = index 1  xs (maximum (take n xs))  --euresh index megaluterou stoixeiou

index n (x:xs) el | x==el = n
                  | otherwise = index (n+1) xs el

flop n xs r | n == 1 = reverse r
            | ((max_index n xs)==n) = flop (n-1) xs r --to stoixeio brisketai sthn swsth thesi
            | ((max_index n xs) /= 1) = flop (n-1) (nreve n (nreve (max_index n xs) xs)) (n:((max_index n xs):r))
            | otherwise = flop (n-1) (nreve n xs) (n:r)

nreve n xs = (reverse (take n xs)) ++ (drop n xs) --antistrofh prwtwn n

naive xs = flop (length xs) xs []

----------------------------------------------------------------visualize--------------------------------------------------------------------------------

visualize xs [] = [xs]
visualize xs (n:ns) = [xs] ++ (visualize (nreve n xs) ns)

----------------------------------------------------------------bfs--------------------------------------------------------------------------------


next_elem f n = [f++[x] | x<-[2..n], next_null f x] --prosthhkh 2,...,n sto telos tou f

next_null [] x = True
next_null f x = x/=(last f)

initial n = [[x] | x<-[2..n]]

checkList [] = True --elegxos an h lista einai ordered
checkList [x] = True
checkList (x:y:xs) = x <= y && checkList (y:xs)

depth (f:fifo) ls visited | goal f ls= f  --brethhke lush
                        | (elem (last (visualize ls f)) visited == True) =  depth fifo ls visited --an uparxei auto to kladi den epekteinetai
                        | otherwise = depth (fifo++(next_elem f (length ls))) ls ((last (visualize ls f)):visited)  --prosthkh sthn fifo

goal a ls= checkList (last (visualize ls a))

bfs ls = depth [[]] ls []

----------------------------------------------------------------bidirectional--------------------------------------------------------------------------------

find_tuple el (x:xs) | el==(fst x) = (snd x)  --epistrofh deuterou stoixeiou
                     | otherwise = find_tuple el xs

d1 (f1:fifo1) fifo2 dep ls visited1 visited2 | length f1 > dep = d2 (f1:fifo1) fifo2 dep  ls visited1 []  --gia megalutero bathos kalei thn dep2
                                           | (length f1 == dep) && (elem (last (visualize ls f1)) ( map (\x->fst x) visited2 )== True) = f1++(reverse (find_tuple (last (visualize ls f1)) visited2))
                                           | otherwise = d1 (fifo1++(reverse (next_elem f1 (length ls)))) fifo2 dep ls (((last (visualize ls f1)),f1):visited1) visited2

d2 fifo1 (f2:fifo2) dep ls visited1 visited2 | length f2 > dep = d1 fifo1 (f2:fifo2) (dep+1)  ls [] visited2
                               | (length f2 == dep) && (elem (last (visualize (sort ls) f2)) ( map (\x->fst x) visited1 )== True) = ((find_tuple (last (visualize (sort ls) f2)) visited1))++( reverse f2)
                               | otherwise = d2 fifo1 (fifo2++(reverse (next_elem f2 (length ls)))) dep ls visited1 (((last (visualize (sort ls) f2)),f2):visited2)

bidirectional ls = d1 [[]] [[]] 0 ls [(ls,[])] [((sort ls),[])]

----------------------------------------------------------------batch--------------------------------------------------------------------------------

how_many el ls = sum (many_lower el ls) --posa stoixeia mikrotera apo to el

many_lower _ [] = []
many_lower el (x:xs) | x<el = [1]++(many_lower el xs)
                     | otherwise = [0]++(many_lower el xs)

positions (x:xs) = map (\y->how_many y (x:xs) ) (x:xs)  --anagwgh se 0,1,...,n-1

change_nth n el (x:xs) | n==1 = el:xs --allagh n-stou stoixeiou
                       | otherwise = x:(change_nth (n-1) el xs)

removeItem _ [] = []   --diagrafh stoixeiou
removeItem x (y:ys) | x==y = removeItem x ys
                    | otherwise = y:removeItem x ys

initial_zero n = [x | x<-[0..n-1]]

goal_lists a ls [] = ([],0) --an uparxei lista pou apo thn arxikh me ton sunduasmo a na ftanoyme se authn
goal_lists a ls (x:xs) | goal_stack a ls (fst x) = x
                       | otherwise = goal_lists a ls xs

goal_stack a ls for_check | ( last (visualize ls a))==for_check = True
                          | otherwise = False

depth_stack (f:fifo) ls visited lists result | lists==[] = result
                                    -- | (elem (last (visualize ls f)) visited == True) =  depth_stack fifo ls visited lists result
                                    | (goal_lists f ls lists)/=([],0) = depth_stack (fifo++(next_elem f (length ls))) ls ((last (visualize ls f)):visited)
                                            ( removeItem (goal_lists f ls lists) lists) --afaireitai h lista pou ikanopoieitai apo to lists kai topotheteitai sto result to f sthn katallhlh thesi tou
                                            (change_nth (snd (goal_lists f ls lists)) (reverse f) result)
                                    | otherwise = depth_stack (fifo++(next_elem f (length ls))) ls ((last (visualize ls f)):visited) lists result

thesis [] n = [] --metatroph apo lista se lista apo tuples me deutero stoixeio thn thesi
thesis (x:xs) n = (x,n):( thesis xs (n+1))

batch (x:xs) = depth_stack [[]] (initial_zero (length x)) [] ( thesis (map (\x->positions x) (x:xs)) 1) (x:xs)

----------------------------------------------------------------burnt--------------------------------------------------------------------------------

--------burnt_naive-------
nth n xs = last (take n xs)

flop_b n xs r | (n == 1 && (snd (head xs)) == 1)= reverse (1:r)
            | (n == 1 && (snd (head xs)) == 0)= reverse r
            | ((max_index n xs)==n) && (snd (nth (max_index n xs) xs) == 0) = flop_b (n-1) xs r
            | ((max_index n xs) /= 1 && (snd (nth (max_index n xs) xs) == 0)) = flop_b (n-1) (burnt_nreve n (burnt_nreve (max_index n xs) xs)) (n:((max_index n xs):r))
            | ((max_index n xs) /= 1 && (snd (nth (max_index n xs) xs) == 1)) = flop_b (n-1) (burnt_nreve n (burnt_nreve 1 (burnt_nreve (max_index n xs) xs))) (n:(1:((max_index n xs):r)))
            | ((max_index n xs) == 1 && (snd (head xs)) == 0) = flop_b (n-1) (burnt_nreve n (burnt_nreve 1 xs)) (n:(1:r))
            | otherwise = flop_b (n-1) (burnt_nreve n xs) (n:r)

burnt_naive xs = flop_b (length xs) xs []

change [] = []  --allagh pleuras olwn
change ((x,1):xs) = ((x,0):(change xs))
change ((x,0):xs) = ((x,1):(change xs))

burnt_nreve n xs = (change (reverse (take n xs))) ++ (drop n xs)
--------burnt_visualize-------

burnt_visualize xs [] = [xs]
burnt_visualize xs (n:ns) = [xs] ++ (burnt_visualize (burnt_nreve n xs) ns)

--------burnt_bfs-------
check_burnt [] = True
check_burnt ((x,y):ls) | y == 0 = check_burnt ls
                       | otherwise = False

goal_b a ls= checkList (last (burnt_visualize ls a)) && check_burnt (last (burnt_visualize ls a))

depth_b (f:fifo) ls visited | goal f ls= f
                        | (elem (last (burnt_visualize ls f)) visited == True) =  depth_b fifo ls visited
                        | otherwise = depth_b (fifo++(next_elem f (length ls))) ls ((last (burnt_visualize ls f)):visited)

burnt_bfs ls = depth_b [[]] ls []

--------burnt_bidirectional-------
zero_tup [] = []
zero_tup ((x,y):xs) = (x,0):(zero_tup xs)

d1_b (f1:fifo1) fifo2 dep ls visited1 visited2 | length f1 > dep = d2 (f1:fifo1) fifo2 dep  ls visited1 []
                                            | (length f1 == dep) && (elem (last (burnt_visualize ls f1)) ( map (\x->fst x) visited2 )== True) = f1++(reverse (find_tuple (last (burnt_visualize ls f1)) visited2))
                                            | otherwise = d1_b (fifo1++( (next_elem f1 (length ls)))) fifo2 dep ls (((last (burnt_visualize ls f1)),f1):visited1) visited2

d2_b fifo1 (f2:fifo2) dep ls visited1 visited2 | length f2 > dep = d1 fifo1 (f2:fifo2) (dep+1)  ls [] visited2
                                | (length f2 == dep) && (elem (last (burnt_visualize (sort (zero_tup ls)) f2)) ( map (\x->fst x) visited1 )== True) = ((find_tuple (last (burnt_visualize (sort (zero_tup ls)) f2)) visited1))++( reverse f2)
                                | otherwise = d2_b fifo1 (fifo2++( (next_elem f2 (length ls)))) dep ls visited1 (((last (burnt_visualize (sort (zero_tup ls)) f2)),f2):visited2)

burnt_bidirectional ls = d1_b [[]] [[]] 0 ls [(ls,[])] [((sort (zero_tup ls)),[])]

--------burnt_batch-------

burn_goal_lists a ls [] = ([],0) --an uparxei lista pou apo thn arxikh me ton sunduasmo a na ftanoyme se authn
burn_goal_lists a ls (x:xs) | burn_goal_stack a ls (fst x) = x
                       | otherwise = burn_goal_lists a ls xs

burn_goal_stack a ls for_check | ( last (burnt_visualize ls a))==for_check = True
                          | otherwise = False

burn_depth_stack (f:fifo) ls visited lists result | lists==[] = result
                                    -- | (elem (last (burnt_visualize ls f)) visited == True) =  depth_stack fifo ls visited lists result
                                    | (burn_goal_lists f ls lists)/=([],0) = burn_depth_stack (fifo++(next_elem f (length ls))) ls ((last (burnt_visualize ls f)):visited)
                                            ( removeItem (burn_goal_lists f ls lists) lists) --afaireitai h lista pou ikanopoieitai apo to lists kai topotheteitai sto result to f sthn katallhlh thesi tou
                                            (change_nth (snd (burn_goal_lists f ls lists)) (reverse f) result)
                                    | otherwise = burn_depth_stack (fifo++(next_elem f (length ls))) ls ((last (burnt_visualize ls f)):visited) lists result

burn_batch (x:xs) = burn_depth_stack [[]] (map (\x-> (x,0)) (initial_zero (length x))) [] ( thesis ( map(\x->zip (positions x) (map (\y->snd y) x) ) (x:xs) ) 1) (map (\x-> positions x) (x:xs) )
