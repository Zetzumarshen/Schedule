{-# LANGUAGE OverloadedStrings #-}

module Tests.Activity where


import Activity 
import Test.QuickCheck
import Data.Time

instance Arbitrary Activity where
    arbitrary = do
        x <- dayGen
        y <- clockGen       
        x1 <- dayGen
        y1 <- clockGen
        return (Activity "test name" "test actor" (UTCTime x y) (UTCTime x1 y1))

clockGen :: Gen DiffTime
clockGen = fmap (\x -> secondsToDiffTime (x * 3600)) $ choose (0,23)

dayGen :: Gen Day
dayGen = fmap (\x -> fromGregorian 2015 x x) $ choose (1, 31)

--commutative
prop_swapisCollision x y = do
    isCollision x y == isCollision y x
    
deepCheck p = quickCheckWith stdArgs { maxSuccess = 10000} p