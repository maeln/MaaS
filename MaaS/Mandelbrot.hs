module MaaS.Mandelbrot (
  mandelbrot,
  mandel_map,
  genImage
  ) where

import Data.Complex
import Data.Word

import MaaS.Maths
import MaaS.Color

mandelbrot :: (RealFloat a, Integral b) => Complex a -> Complex a -> b -> b -> b
mandelbrot _ _ 0 _ = 0
mandelbrot z c mx n | (realPart z)**2 + (imagPart z)**2 <= 4 = mandelbrot (z*z+c) c (mx-1) (n+1)
                    | otherwise = n

mandel_map :: (Enum b, Integral a, RealFloat b) => a -> b -> b -> b -> b -> b -> [a]
mandel_map m ptx pty x y z = map (\ n -> mandelbrot (0:+0) n m 0) (concat (genComplexSpace ptx pty x y z))

genImage :: Integral b => [b] -> b -> [(Word8,Word8,Word8)]
genImage space m = map (convColor . genColor [
                           Color 0.0 0.0 1.0,
                           Color 0.0 1.0 0.0,
                           Color 1.0 0.0 0.0
                           ] m) space
