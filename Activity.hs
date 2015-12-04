module Activity 
  ( Activity(..)
  , isCollision
  ) where
  
import Data.Time 
 
data Activity = Activity
  { name :: String
  , actor :: Actor
  , timeStart :: UTCTime
  , timeEnd :: UTCTime
  } deriving Show
    
type Actor = String


isCollision :: Activity -> Activity -> Bool
isCollision (Activity _ act1 st1 end1) (Activity _ act2 st2 end2) =
    act1 == act2 && st1 < end1 && st2 < end1