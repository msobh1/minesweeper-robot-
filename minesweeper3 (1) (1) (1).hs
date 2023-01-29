type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show ,Eq)
up:: MyState -> MyState
up (S(0,y) (xs:ys) s a) = Null 
up (S(x,y) (xs:ys) s a) = (S(x-1,y) (xs:ys) "up" (S (x,y) (xs:ys) s a))

down:: MyState -> MyState
down (S(3,y) (xs:ys) s a)  = Null
down (S(x,y) (xs:ys) s a ) = (S(x+1,y) (xs:ys) "down" (S (x,y) (xs:ys) s a))

left:: MyState -> MyState
left (S(x,0) (xs:ys) s a) = Null
left (S(x,y) (xs:ys) s a)= (S(x,y-1) (xs:ys)   "left" (S (x,y) (xs:ys) s a))

right:: MyState -> MyState
right (S(x,3) (xs:ys) s a)  = Null
right (S(x,y) (xs:ys) s a) = (S(x,y+1) (xs:ys) "right" (S (x,y) (xs:ys) s a))
found :: Cell ->[Cell]->Bool
found x []  = False
found x (xs:ys) |x==xs=True
                |otherwise = found x ys
remove::Cell->[Cell]->[Cell]
remove x []=[]
remove  x (xs:ys)|x==xs=remove x ys
                  |otherwise = xs:remove x ys
collect:: MyState -> MyState
collect (S x (xs:ys) s a)| found x (xs:ys) = (S x (remove x (xs:ys)) "collect" (S x (xs:ys) s a))
                         | otherwise = (S x (xs:ys) s a )

nextMyStates::MyState->[MyState]
nextMyStates state=filter (/=Null) ((collect state):(up state):(down state):(left state):(right state):[])
isGoal::MyState->Bool
isGoal (S x y s a) | y==[]=True
                   | otherwise=False				 
search::[MyState]->MyState
search (x:z)| isGoal(x) = x
              | otherwise = search (z++(nextMyStates x))
constructSolution:: MyState ->[String]
constructSolution Null = []
constructSolution (S (x,y) l "" k) = constructSolution k
constructSolution (S (x,y) l b k) = (constructSolution k) ++[b]
solve :: Cell->[Cell]->[String]
solve1:: MyState -> [String]
solve x y = solve1 (S x y "" Null)
solve1 a = constructSolution(search( [a]))




				


