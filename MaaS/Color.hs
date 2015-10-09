module MaaS.Color (
  Color(..),
  genColor,
  getColor,
  convColor,
  colorInterpol
  ) where

import Data.Word
import MaaS.Tools
import MaaS.Maths

data Color a = Color a a a deriving Show

getColor :: RealFrac a => [Color a] -> a -> Color a
getColor cols r = colorInterpol (cols !! (n-1)) (cols !! n) (remap 0 1 p1 p2 r)
  where n = clamp 1 ((length cols)-1) (round ((fromIntegral (length cols))*r))
        p1 = (fromIntegral (n-1)) / (fromIntegral (length cols))
        p2 = (fromIntegral (n)) / (fromIntegral (length cols))


genColor :: (RealFrac a, Integral b) => [Color a] -> b -> b -> Color a
genColor cols m n
  | n == 0    = (Color 0 0 0)
  | otherwise = getColor cols (dn/dm)
  where dm = fromIntegral m
        dn = fromIntegral n

convColor :: RealFrac a => Color a -> (Word8,Word8,Word8)
convColor (Color r g b) = (w8 (r*255.0),w8 (g*255.0),w8 (b*255.0))
  where w8 = floor


colorInterpol :: RealFrac a => Color a -> Color a -> a -> Color a
colorInterpol (Color r1 g1 b1) (Color r2 g2 b2) m = (Color (li r1 r2 m) (li g1 g2 m) (li b1 b2 m))
  where li = linearInterpol

