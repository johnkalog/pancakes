
category (l:ls) | (ls == []) = 8
                | ((length l) == 1) && ( ((next_abj ((head l)+1) ls) == 1) || ((next_abj ((head l)-1) ls) == 1) ) = 1
                | ((length l) == 1) && ((next_abj ((head l)+1) ls) > 1 || (next_abj ((head l)-1) ls) > 1) = 2
                | ((length l) == 1) = 3
                | ((length l) > 1 &&  (((next_abj ((head l)+1) ls) == 1) || ((next_abj ((head l)-1) ls) == 1))) = 4
                | ((length l) > 1 && (is_pos head ((head l)+1) ls || is_pos head ((head l)-1) ls)) = 5
                | ((length l) > 1 && (is_pos last ((head l)-1) ls) && ((next_abj ((last l)+1) ls) == 1)) = 6
                | ((length l) > 1 && (is_pos last ((head l)-1) ls) && ((next_abj ((last l)+1) ls) > 1)) = 7
                | otherwise = 10


next_abj x [] = 0
next_abj x (l:ls) | ((head l) == x) = (length l)
                | otherwise = next_abj x ls

is_pos f x [] = False
is_pos f x (l:ls) | (length l == 1) = is_pos f x ls
                  | ((f l) == x) = True
                  | otherwise = is_pos f x ls
my_merge [] = []
my_merge (l1:(l2:l)) | ((last l1)+1 == (head l2)) =  ((l1++(head (my_merge (l2:l)))):(delete_firtst (my_merge (l2:l))))
                     | otherwise = (l1:my_merge (l2:l))

my_merge [x] = [x]

delete_firtst (l:ls) = ls
