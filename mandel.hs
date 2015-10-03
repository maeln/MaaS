import Codec.PPM.Binary
import System.Environment
import Data.Complex
import Data.Matrix
import Data.Word

{-
c = -0.1528 + 1.0397i
x = [-2;1]
y = [-1;1]
mapping matrix (x,y):
tx = -2 + 3/x * dx
ty = 1 -2/y * dy

150 200
-}

-- Space / Mathy stuff 
switchSpace :: (RealFrac a, Integral b) => (b,b) -> (b,b) -> Complex a 
switchSpace (x,y) (dx,dy) = ( (-2 + 3/fi(x) * fi(dx)) :+ (1 - 2/fi(y) * fi(dy)) ) 
                            where fi = fromIntegral

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

genImage :: Integral a => Matrix a -> a -> [(Word8,Word8,Word8)]
genImage matrix m = map (convColor . genColor [Color 0.2 0.3 0.4, Color 0.85 0.35 0.31, Color 0.3 0.5 0.8] m) (toList (transpose matrix))

convColor :: RealFrac a => Color a -> (Word8,Word8,Word8)
convColor (Color r g b) = (w8 (r*255.0),w8 (g*255.0),w8 (b*255.0))
  where w8 = floor

-- Mandelbrot

{-
mandelbrot :: (RealFloat a, Integral b) => Complex a -> Complex a -> b -> b
mandelbrot origin ci mx = aux origin ci mx
            where aux _ _ 0  = 0
                  aux z c n | magnitude z <= 2 = aux (z*z+c) c (n-1)
                  aux z _ n | magnitude z > 2 = n
-}

mandelbrot :: (RealFloat a, Integral b) => Complex a -> Complex a -> b ->b -> b
mandelbrot _ _ 0 _ = 0
mandelbrot z c mx n | (realPart z)**2 + (imagPart z)**2 <= 4 = mandelbrot (z*z+c) c (mx-1) (n+1)
                    | otherwise = n

mandelMapper :: Integral a => a -> (Int,Int) -> (Int,Int) -> a
mandelMapper mx size pos = mandelbrot (0 :+ 0) (switchSpace size pos) mx 0

mandelMatrix :: Integral a => a -> Int -> Int -> Matrix a
mandelMatrix mx x y = matrix x y (mandelMapper mx (x,y))

main = do
  [f,m,x,y] <- getArgs
  let fm = read m :: Int in
    let fx = read x :: Int in
    let fy = read y :: Int in
    writePPM f (fromIntegral fx, fromIntegral fy) (genImage (mandelMatrix fm fx fy) fm)


