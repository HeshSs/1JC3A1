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
cubicQ a b c = ((3 * a * c) - (b**2)) / (9 * (a**2)) 

{- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 - Description: TODO add comments on cubicR here 
 -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = ((9 * a * b * c) - (27 * (a**2) * d) - (2 * (b**3))) / (54 * (a**3))

{- -----------------------------------------------------------------
 - cubicDisc
 - -----------------------------------------------------------------
 - Description: TODO add comments on cubicDisc here 
 -}
cubicDisc :: Double -> Double -> Double
cubicDisc q r = ((q**3) + (r**2))

{- -----------------------------------------------------------------
 - cubeRoot
 - -----------------------------------------------------------------
 - Description: TODO add comments on cubicS here 
 -}
cubeRoot :: Double -> Double
cubeRoot a = (a**(1/3))

{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - Description: TODO add comments on cubicS here 
 -}
cubicS :: Double -> Double -> Double
cubicS q r = cubeRoot (r + sqrt(cubicDisc q r))

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description: TODO add comments on cubicT here 
 -}
cubicT :: Double -> Double -> Double
cubicT q r = cubeRoot (r - sqrt(cubicDisc q r))

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

-- TODO Add Test Cases for each of your functions below here

{-
if ((cubicDisc (cubicQ a b c) (cubicR a b c d)) < 0.0)
                                then []
                                else if ((cubicDisc (cubicQ a b c) (cubicR a b c d)) < 0.1e-10 && (cubicDisc (cubicQ a b c) (cubicR a b c d)) > -0.1e-10)
                                    then [x1, x2, x2]
                                    else [x1]
                                        where    x1 = (cubicS (cubicQ a b c) (cubicR a b c d)) + (cubicT (cubicQ a b c) (cubicR a b c d)) - (b / (3 * a))
                                                 x2 = (- ((cubicS (cubicQ a b c) (cubicR a b c d)) + (cubicT (cubicQ a b c) (cubicR a b c d)))/2) - (b / (3*a)) 
-}