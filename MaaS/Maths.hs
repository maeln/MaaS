module MaaS.Maths (
  genComplexSpace,
  linearInterpol
  ) where

import Data.Complex

genComplexSpace :: (Enum a, RealFloat a) => a -> a -> a -> a -> a -> [[Complex a]]
genComplexSpace xpt ypt resx resy zoom = [ [((x*r+(xpt-(resx/2)*r)) :+ (y*r+(ypt-(resy/2)*r))) | x <- [0.0,1.0..(resx-1)]] | y <- [0.0,1.0..(resy-1)]]
  where r = 1/(zoom*(resx/3.0))
  
linearInterpol :: Num a => a -> a -> a -> a
linearInterpol x y r = x * (1-r) + y * r

