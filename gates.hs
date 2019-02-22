
category (l:ls) | (ls == []) = 8
                | ((length l) == 1) && ( ((next_abj ((head l)+1) ls) == 1) || ((next_abj ((head l)-1) ls) == 1) ) = 1
                | ((length l) == 1) && ((next_abj ((head l)+1) ls) > 1 || (next_abj ((head l)-1) ls) > 1) = 2
                | ((length l) == 1) = 3
                | ((length l) > 1 &&  (((next_abj ((head l)+1) ls) == 1) || ((next_abj ((head l)-1) ls) == 1))) = 4
                | ((length l) > 1 && (is_pos head ((head l)+1) ls || is_pos head ((head l)-1) ls)) = 5
                | ((length l) > 1 && (is_pos last ((head l)-1) ls) && ((next_abj ((last l)+1) ls) == 1)) = 6
                | ((length l) > 1 && (is_pos last ((head l)-1) ls) && ((next_abj ((last l)+1) ls) > 1)) = 7
                | otherwise = 10


gates (l:ls)
            | category (l:ls) == 1 = my_merge ( my_flip (l:ls) ((which_flip (l:ls) (head l) 1) -1))
            | category (l:ls) == 2 = my_merge ( my_flip (l:ls) ((which_flip (l:ls) (head l) 1) -1))
            | category (l:ls) == 3 = my_merge ( my_flip (l:ls) (min ((which_exac (l:ls) ((head l)-1) 1) -1) (which_exac (l:ls) ((head l)-1) 1) -1))
            | otherwise = [[category (l:ls)]]

next_abj x [] = 0
next_abj x (l:ls) | ((head l) == x) = (length l)
                | otherwise = next_abj x ls

is_pos f x [] = False
is_pos f x (l:ls) | (length l == 1) = is_pos f x ls
                  | ((f l) == x) = True
                  | otherwise = is_pos f x ls
my_merge [] = []
my_merge (l1:(l2:l)) | ((last l1)+1 == (head l2) || (last l1) == (head l2)+1) =  ((l1++(head (my_merge (l2:l)))):(delete_firtst (my_merge (l2:l))))
                     | otherwise = (l1:my_merge (l2:l))

my_merge [x] = [x]

delete_firtst (l:ls) = ls

inside_flip ls 0 = []
inside_flip (l:ls) n = ((reverse l):(inside_flip ls (n-1)))

my_flip ls n = reverse (inside_flip ls n) ++ (drop n ls)


which_flip [] _ _ = 0
which_flip (x:xs) el n | (elem (el+1) x) || (elem (el-1) x) = n
                       | otherwise = which_flip xs el (n+1)


which_exac [] _ _ = 0
which_exac (x:xs) el n | (elem el x)  = n
                       | otherwise = which_exac xs el (n+1)
