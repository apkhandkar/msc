ll : [Int] -> Int
ll . []       = 0
ll . (x::xs)  = 1 + ll.xs

lll : [[Int]] -> Int
lll . []      = 0
lll . (x::xs) | ll.x > lll.xs = ll.x
              | otherwise     = lll.xs

conv2tup : [Int] -> [(Int,Int)]
conv2tup . []       = []
conv2tup . (x::xs)  = (x,(ll.(x::xs)-1)) :: conv2tup.xs

mkzeros : Int -> [Int]
mkzeros . 0 = []
mkzeros . n = 0 :: mkzeros.(n-1)

fel : (a,a) -> a
fel . (x,_) = x

sel : (a,a) -> a
sel . (_,y) = y

tupMult : (Int,Int) -> [Int] -> [Int]
tupMult . t . l = [(fel.t*x)|x<-l] ++ mkzeros.(sel.t)

trmMult : [(Int,Int)] -> [Int] -> [[Int]]
trmMult . tups . l = [(tupMult.x.l)|x<-tups]

padleft : Int -> [Int] -> [Int]
padleft . 0 . l = l
padleft . n . l = 0 :: padleft.(n-1).l

eqlsts : [[Int]] -> [[Int]]
eqlsts . lst = [padleft.(lll.lst-ll.t).t|t<-lst]

lstMult : [Int] -> [Int] -> [[Int]]
lstMult . l1 . l2 =  eqlsts.(trmMult.(conv2tup.l1).l2)

eqilst : [[Int]] -> Bool
eqilst . []                             = True
eqilst . (x::(y::ys))   | ll.x == ll.y  = eqilst.ys
                        | otherwise     = False

hdsum : [[Int]] -> Int
hdsum . []            = 0
hdsum . ((x::xs)::ys) = x + hdsum.(ys)

lsthd : [Int] -> Int
lsthd . []      = 0
lsthd . (x::xs) = x

lsttl : [Int] -> [Int]
lsttl . []      = []
lsttl . (x::xs) = xs

gethds : [[Int]] -> [Int]
gethds . []       = []
gethds . (x::xs)  = lsthd.x :: gethds.xs

gettls : [[Int]] -> [[Int]]
gettls . []       = []
gettls . (x::xs)  = lsttl.x :: gettls.xs


trpls : [[Int]] -> [[Int]]
trpls . ([]::_) = []
trpls . x       = gethds.x :: trpls.(gettls.x)

sumls : [Int] -> Int
sumls . []      = 0
sumls . (x::xs) = x + sumls.xs

smlls : [[Int]] -> [Int]
smlls . []      = []
smlls . (x::xs) = sumls.x :: smlls.xs

plmlt : [Int] -> [Int] -> [Int]
plmlt . l1 . l2 = smlls.(trpls.(lstMult.l1.l2))



transfer : [Int] -> [Int]
transfer . []                         = []
transfer . (x::[])  | (0<=x)&&(x<=9)  = [x]
                    | otherwise       = x`mod`10 :: transfer.[((x-(x`mod`10))/10)]
transfer . (x::(y::ys))               = x`mod`10 :: transfer.((((x-(x`mod`10))/10)+y)::ys)

lrev : [Int] -> [Int]
lrev . []       = []
lrev . (x::xs)  = lrev.xs ++ [x]

decfix : [Int] -> [Int]
decfix . l = lrev.(transfer.(lrev.l))

simult_int : [Int] -> [Int] -> [Int]
simult_int . l1 . l2 = decfix.(plmlt.l1.l2)

s2ilist : [Char] -> [Int]
s2ilist . ['0']   = [0]
s2ilist . ['1']   = [1]
s2ilist . ['2']   = [2]
s2ilist . ['3']   = [3]
s2ilist . ['4']   = [4]
s2ilist . ['5']   = [5]
s2ilist . ['6']   = [6]
s2ilist . ['7']   = [7]
s2ilist . ['8']   = [8]
s2ilist . ['9']   = [9]
s2ilist . (x::xs) = s2ilist.[x] ++ s2ilist.xs

ilist2s : [Int] -> [Char]
ilist2s . [0]   = ['0']
ilist2s . [1]   = ['1']
ilist2s . [2]   = ['2']
ilist2s . [3]   = ['3']
ilist2s . [4]   = ['4']
ilist2s . [5]   = ['5']
ilist2s . [6]   = ['6']
ilist2s . [7]   = ['7']
ilist2s . [8]   = ['8']
ilist2s . [9]   = ['9']
ilist2s . (x::xs) = ilist2s.[x] ++ ilist2s.xs

bfmul : [Char] -> [Char] -> [Char]
bfmul . n1 . n2 = ilist2s.(simult_int.(s2ilist.n1).(s2ilist.n2))








