module TehtParit where

x = (True,42,10)

fstOfTriple :: (Bool,Int,Int) -> Bool
fstOfTriple kolmikko 
    = case kolmikko of 
        (bool,i,is) -> bool

sndOfTriple :: (Bool,Int,Int) -> Int
sndOfTriple kolmikko 
    = case kolmikko of 
        (bool,i,is) -> i

thdOfTriple :: (Bool,Int,Int) -> Int
thdOfTriple kolmikko 
    = case kolmikko of 
        (bool,i,is) -> is

paritTeht3 :: (Bool, Int, Int) -> (Bool, Int)
paritTeht3 kolmikko  
    = case kolmikko of
        (bool,i,is) -> (bool,(i+is)) 