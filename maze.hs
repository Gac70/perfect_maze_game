import System.Random
import System.IO.Unsafe

-- Ekswteriki vivliothiki
import Data.List

data Maze = Maze { cells :: [(Bool, Bool)]  -- [(rightWall, downWall)]
                 , width :: Int
                 , height :: Int
                 }

rand :: Int -> Int

-- Returns a random integer from 0 to max-1
rand max = unsafePerformIO $ randomRIO (0, max-1)

shuffle :: [a] -> [a]
-- Randomly shuffles a list
shuffle = unsafePerformIO . shuffleM

shuffleM :: [a] -> IO [a]
-- DON'T BOTHER! Helper for shuffle
shuffleM [] = return []
shuffleM n = do {
                r <- fmap (flip mod $ length n) randomIO;
                n1 <- return $ n !! r;
                fmap ((:) n1) $ shuffleM $ (take r n) ++ (drop (r+1) n)
             }



--------------------------------------------------------------------------------

select :: Int -> [a] -> a

select 1 (x:xs) =x
select n (x:xs) = select (n-1) xs

makeallWalls :: Int -> [(Bool,Bool)]
makeallWalls 0 = []
makeallWalls n = [(True,True)]++makeallWalls (n-1)


makeMaze :: Int -> Int -> Maze
makeMaze width height = (Maze (makeallWalls (width*height)) width height)




rightWalls :: Int -> Int -> Int -> [(Int,Int)]
rightWalls width height c =  if (c==(width *height )) then [] else if ((rem c  width)==0) then rightWalls width height (c+1) else [(c,c+1)]++rightWalls width height (c+1)
downWalls :: Int -> Int -> Int -> [(Int,Int)]
downWalls width height c =if (c>((width*height)-width)) then [] else [(c,(c+width))]++downWalls width height (c+1)

everyWalls :: Int -> Int -> [(Int,Int)]
everyWalls width height = rightWalls width height 1 ++ downWalls width height 1






makeSetsh :: Int -> [[Int]]
makeSetsh 0 = []
makeSetsh n = [[n]]++makeSetsh (n-1)

makeSets :: Int -> [[Int]]
makeSets n = reverse (makeSetsh n)



member :: Int -> [Int] -> Bool
member x [] = False
member x (y:xs) = if (x==y) then True
                  else (member x xs)


--delete apo Data.List


--del :: a->[a]->[a]
--del x [] = []
--del x (y:xs) = if (x==y) then (del x xs) else (y:del x xs)
--n = number of cells
--allcellsWalls :: Int Int Int -> [(Int,Int)]

--allcellsWalls width height c = if ((c rem width )==0) then [(c,c+width)]++allcellsWalls width height c+1 else if ( c )

replace :: Int -> [a]-> [[a]]->[[a]]
replace i joined_set sets  = take (i-1) sets ++ [joined_set] ++ drop (i) sets 


replacecell :: Int -> (Bool,Bool)-> [(Bool,Bool)]->[(Bool,Bool)]
replacecell i cell cellset  = take (i-1) cellset ++ [cell] ++ drop (i) cellset 



newsets :: [Int] -> [Int] -> [[Int]]-> [[Int]]
newsets js [] sets = sets
newsets js (x:joined_set) sets = newsets js joined_set (replace x js sets)

newset :: [Int] ->  [[Int]]-> [[Int]]

newset js sets = newsets js js sets

--road

roadtoCell :: (Int,Int) -> (Bool,Bool) -> (Bool,Bool)

roadtoCell  (ci,cj) (True,True) = if (cj==ci+1) then (False,True) else (True,False) 
roadtoCell  (ci,cj) (True,False) = if(cj==ci+1) then (False,False)  else (True,False)
roadtoCell  (ci,cj) (False,True) =if(cj==ci+1) then (False,True)  else (False,False)
roadtoCell  (ci,cj) (False,False) =(False,False)

roadstoCells :: [(Int,Int)] -> [(Bool,Bool)] -> [(Bool,Bool)]

roadstoCells [] cells = cells
roadstoCells  ((ci,cj):roads) cells = roadstoCells roads (replacecell ci (roadtoCell (ci,cj) (select ci cells)) cells)




kruskalfor :: [(Int,Int)] -> [[Int]] -> [(Int,Int)] 

kruskalfor [] sets = []
kruskalfor ((ci,cj):swalls) sets = if(( (member ci (select cj sets))==False ) && ( (member cj (select ci sets))==False ) ) 
                                   then  [(ci,cj)]++kruskalfor swalls (newset (union (select cj sets) (select ci sets)) sets )
                                   else  kruskalfor swalls sets

                                   --kruskalfor swalls (newsets [] (union  (select ci sets) (select cj sets)) sets)  else [(ci,cj)]++kruskalfor swalls sets


kruskal :: Maze -> Maze

kruskal m = (Maze (roadstoCells (
                          kruskalfor (
                             shuffle (
                              everyWalls (width m) (height m))
                          ) 
                           (makeSets (
                          (width m) * (height m)))) (cells m)
                 )
                  (width m) (height m)
            )

--kruskalfor (shuffle (everyWalls (width m) (height m))) (makeSets ((width m) * (height m)))) (cells m)


printfirstline :: Int -> [Char]
printfirstline 0 = "+\n"
printfirstline width = "+----"++printfirstline (width-1)


printrightcell (True,_) = "|    "
printrightcell x = "     "

printdowncell  (_,True) = "----+"
printdowncell  (_,False) = "    +"

printrightcells (cells) 1 = []
printrightcells (c:cells) n =  printrightcell(c)++printrightcells cells (n-1)

printdowncells (cells) 0 = []
printdowncells (c:cells) n =  printdowncell(c)++printdowncells cells (n-1)


printrightline cells width = "|    "++printrightcells cells width ++"|\n"
printdownline cells width = "+"++printdowncells cells width++"\n"



printm [] width =[]
printm cells width = printrightline cells width++printdownline cells width++printm (drop width cells ) width

printmaze :: Maze -> [Char]
printmaze m = ((printfirstline (width m))++(printm (cells m) (width m) ))



showMaze :: Maze -> [(Int,Int)] -> String

showMaze m kelia = printmaze m 


--pos :: Maze->(Int,Int)->Int
--pos  m (x,y) = (x-1)*(width m)+y


--go R m (x,y) = if(head (select (pos m (x,y)) cells m))==True) then 
 
--solvePerfect :: Maze -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
--solvePerfect m  (sx,sy) (gx,gy) = if(sx==gx && sy==gy) then [] else go R m (sx,sy) ++ go D m (sx,sy) 





