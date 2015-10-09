module MaaS.Maths (
  genComplexSpace,
  linearInterpol,
  spaceByCorner
  ) where

import Data.Complex

genComplexSpace :: (Enum a, RealFloat a) => a -> a -> a -> a -> a -> [Complex a]
genComplexSpace xpt ypt resx resy zoom = concat [ [((x*r+offx) :+ (y*r+offy)) | x <- [0.0,1.0..(resx-1)]] | y <- [0.0,1.0..(resy-1)]]
  where r = 1/(zoom*(resx/3.0))
        offx = (xpt-(resx/2)*r)
        offy = (ypt-(resy/2)*r)

spaceByCorner :: (Enum a, RealFloat a) => a -> a -> a -> a -> a -> a -> [Complex a]
spaceByCorner x0 y0 x1 y1 rx ry = [(x:+y) | y <- [y0,(y0+dy)..y1], x <- [x0,(x0+dx)..x1]]
  where dx = (x1-x0)/(rx-1)
        dy = (y1-y0)/(ry-1)
  
linearInterpol :: Num a => a -> a -> a -> a
linearInterpol x y r = x * (1-r) + y * r

