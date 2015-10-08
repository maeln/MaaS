import Codec.PPM.Binary
import System.Environment
import Data.Complex
import Data.Matrix
import Data.Word

import Control.Parallel
import Control.Parallel.Strategies

-- Space / Mathy stuff 
switchSpace :: (RealFrac a, Integral b) => (b,b) -> (b,b) -> Complex a 
switchSpace (x,y) (dx,dy) = ( (-2 + 3/fi(x) * fi(dx)) :+ (1 - 2/fi(y) * fi(dy)) ) 
                            where fi = fromIntegral
{- A PROPOS :
Pour le moment, switchSpace fonction en donnant les bornes de l'espace,
à l'avenir, ce qu'il faudrait faire c'est donnée un point dans le plan complexe
un delta X,Y pour chaque pixel, et determiner l'espace couvert avec la résolution donnée.
-}

toMandelbrotSpace :: (RealFrac a, Integral b) => (a,a) -> (b,b) -> (b,b) -> Complex a
toMandelbrotSpace (dx,dy) (maxi,mayi) (x,y) = (rx*dx :+ ry*dy)
  where mx = maxi `div` 2
        my = mayi `div` 2
        rx = fromIntegral (x - mx)
        ry = fromIntegral (y - my)

genComplexSpace :: (Enum a, RealFloat a) => a -> a -> a -> a -> a -> [[Complex a]]
genComplexSpace xpt ypt resx resy zoom = [ [((xpt+x*r) :+ (ypt+y*r)) | x <- [0.0,1.0..(resx-1)]] | y <- [0.0,1.0..(resy-1)]]
  where r = 1/(zoom*(resx/3.0))
  
linearInterpol :: Num a => a -> a -> a -> a
linearInterpol x y r = x * (1-r) + y * r

colorInterpol :: RealFrac a => Color a -> Color a -> a -> Color a
colorInterpol (Color r1 g1 b1) (Color r2 g2 b2) m = (Color (li r1 r2 m) (li g1 g2 m) (li b1 b2 m))
  where li = linearInterpol

clamp :: Ord a => a -> a -> a -> a
clamp mn mx v = (max mn . min mx) v

remap :: RealFrac a => a -> a -> a -> a -> a -> a
remap x imin imax omin omax = (x-imin)*(omax-omin)/(imax-imin)+omin

-- Color
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

-- Mandelbrot

mandelbrot :: (RealFloat a, Integral b) => Complex a -> Complex a -> b -> b -> b
mandelbrot _ _ 0 _ = 0
mandelbrot z c mx n | (realPart z)**2 + (imagPart z)**2 <= 4 = mandelbrot (z*z+c) c (mx-1) (n+1)
                    | otherwise = n

genImage :: Integral b => [b] -> b -> [(Word8,Word8,Word8)]
genImage space m = map (convColor . genColor [Color 0.85 0.35 0.31, Color 0.3 0.5 0.8] m) space

mandel_map :: (NFData a, Enum b, Integral a, RealFloat b) => a -> b -> b -> b -> b -> b -> [a]
mandel_map m ptx pty x y z = (parMap rdeepseq) (\ n -> mandelbrot (0:+0) n m 0) (concat (genComplexSpace ptx pty x y z))

main = do
  [f,m,ptx,pty,x,y,z] <- getArgs
  let fm = read m :: Int in
    let px = read ptx :: Double in
    let py = read pty :: Double in
    let fx = read x :: Integer in
    let fy = read y :: Integer in
    let fz = read z :: Double in
    writePPM f (fx, fy) (genImage (mandel_map fm px py (fromIntegral fx) (fromIntegral fy) fz) fm)


