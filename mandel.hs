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
switchSpace :: (Int,Int) -> (Int,Int) -> Complex Double 
switchSpace (x,y) (dx,dy) = ( (-2 + 3/fi(x) * fi(dx)) :+ (1 - 2/fi(y) * fi(dy)) ) 
                            where fi = fromIntegral

linearInterpol :: Num a => a -> a -> a -> a
linearInterpol x y r = x * (1-r) + y * r

colorInterpol :: Color -> Color -> Float -> Color
colorInterpol (Color r1 g1 b1) (Color r2 g2 b2) m = (Color (li r1 r2 m) (li g1 g2 m) (li b1 b2 m))
  where li = linearInterpol

clamp :: Int -> Int -> Int -> Int
clamp mn mx v = (max mn . min mx) v

remap :: Float -> Float -> Float -> Float -> Float -> Float
remap x imin imax omin omax = (x-imin)*(omax-omin)/(imax-imin)+omin

-- Color
data Color = Color Float Float Float deriving Show

getColor :: [Color] -> Float -> Color
getColor cols r = colorInterpol (cols !! (n-1)) (cols !! n) (remap 0 1 p1 p2 r)
  where n = clamp 1 ((length cols)-1) (round ((fromIntegral (length cols))*r))
        p1 = (fromIntegral (n-1)) / (fromIntegral (length cols))
        p2 = (fromIntegral (n)) / (fromIntegral (length cols))


genColor :: [Color] -> Int -> Int -> Color
genColor cols m n
  | n == 0    = (Color 0 0 0)
  | otherwise = getColor cols (dn/dm)
  where dm = fromIntegral m
        dn = fromIntegral n

genImage :: Matrix Int -> Int -> [(Word8,Word8,Word8)]
genImage matrix m = map (convColor . genColor [Color 0.2 0.3 0.4, Color 0.85 0.35 0.31, Color 0.3 0.5 0.8] m) (toList (transpose matrix))

convColor :: Color -> (Word8,Word8,Word8)
convColor (Color r g b) = (w8 (r*255.0),w8 (g*255.0),w8 (b*255.0))
  where w8 = round

-- Mandelbrot

mandelbrot :: Complex Double -> Complex Double -> Int -> Int
mandelbrot origin c max = aux origin c max
            where aux z c 0  = 0
                  aux z c n | magnitude z <= 2.0 = aux (z*z+c) c (n-1)
                  aux z c n | magnitude z > 2.0 = n

mandelMapper :: Int -> (Int,Int) -> (Int,Int) -> Int
mandelMapper max size pos = mandelbrot (0.0 :+ 0.0) (switchSpace size pos) max

mandelMatrix :: Int -> Int -> Int -> Matrix Int
mandelMatrix max x y = matrix x y (mandelMapper max (x,y))

toCSV :: Matrix Int -> Int -> Int -> String
toCSV matrix x y = aux 1 1
                   where aux mx my | mx <= x && my <= y = show (matrix ! (mx,my)) ++ ";" ++ aux (mx+1) my
                         aux mx my | mx > x  && my <= y = "\n" ++ aux 1 (my+1)
                         aux mx my | my > y  = ""

{-
genColor :: [Color] -> Int -> Int -> Color
genColor cols m n = colorInterpol (cols !! 0) (cols !! 1) c
                 where c = ((fromIntegral m) / (fromIntegral n))
-}

main = do
  [f,m,x,y] <- getArgs
  let fm = read m :: Int in
    let fx = read x :: Int in
    let fy = read y :: Int in
    writePPM f (fromIntegral fx, fromIntegral fy) (genImage (mandelMatrix fm fx fy) fm)


