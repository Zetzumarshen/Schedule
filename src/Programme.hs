module Programme where

import Activity (Activity)
import Data.List (intersectBy)

data Programme = Programme
  { name :: String
  , acts :: [Activity]
  }
  
getCollision :: Programme -> [Programme] -> [Activity]
getCollision _ [] = []
getCollision pCont (prog:progs) =
    (intersectBy isCollision (acts pCont) (acts prog)) ++ getCollision pCont progs
    
