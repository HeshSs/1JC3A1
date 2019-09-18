{- Assignment 1
 - Name: Hishmat Salehi
 - Date: 19/9/2019
 -}
module Assign_1 where

macid :: String
macid = "Salehh6"

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - Description: TODO add comments on cubicQ here 
 -}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = (((3 * a * c) - (b**2)) / (9 * (a**2)))

{- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 - Description: TODO add comments on cubicR here 
 -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = (((9 * a * b * c) - (27 * (a**2) * d) - (2 * (b**3))) / (54 * (a**3)))

{- -----------------------------------------------------------------
 - cubicDisc
 - -----------------------------------------------------------------
 - Description: TODO add comments on cubicDisc here 
 -}
cubicDisc :: Double -> Double -> Double
cubicDisc q r = (q**3) + (r**2)

{- -----------------------------------------------------------------
 - cubeRoot
 - -----------------------------------------------------------------
 - Description: TODO add comments on cubicS here 
 -}
cubeRoot :: Double -> Double
cubeRoot a = if (a < 0.0)
                then  -((-a)**(1/3))
                else (a)**(1/3)

{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - Description: TODO add comments on cubicS here 
 -}
cubicS :: Double -> Double -> Double
cubicS q r = cubeRoot (r + sqrt (cubicDisc q r) )

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description: TODO add comments on cubicT here 
 -}
cubicT :: Double -> Double -> Double
cubicT q r = cubeRoot (r - sqrt (cubicDisc q r) )

{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description: TODO add comments on cubicRealSolutions here 
 -}
cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]
cubicRealSolutions a b c d = if ((cubicDisc (cubicQ a b c) (cubicR a b c d)) < 0.0)
                                then []
                                else if ((cubicDisc (cubicQ a b c) (cubicR a b c d)) > 0.0)
                                    then [x1]
                                    else [x1, x2, x2]
                                        where    x1 = (cubicS (cubicQ a b c) (cubicR a b c d)) + (cubicT (cubicQ a b c) (cubicR a b c d)) - (b / (3 * a))
                                                 x2 = (-((cubicS (cubicQ a b c) (cubicR a b c d)) + (cubicT (cubicQ a b c) (cubicR a b c d)))/2) - (b / (3 * a))

{- -----------------------------------------------------------------
 - Test Cases 
 - -----------------------------------------------------------------
 -}

test1 = cubicRealSolutions 1 2 3 4 == [-1.6506291914393882]
test2 = cubicRealSolutions 1 (-2) 0 1 == []
test3 = cubicRealSolutions 2 (-4) (-22) 24 == []
test4 = cubicRealSolutions 1 0 3 0 == [0.0]
test5 = cubicRealSolutions 1 2 0 0 == [-2.0,0.0,0.0]
test6 = cubicRealSolutions 1 2 22 0 == [0.0]