import Codec.PPM.Binary
import System.Environment

import MaaS.Mandelbrot

main = do
  [f,m,ptx,pty,x,y,z] <- getArgs
  let fm = read m :: Int in
    let px = read ptx :: Double in
    let py = read pty :: Double in
    let fx = read x :: Integer in
    let fy = read y :: Integer in
    let fz = read z :: Double in
    writePPM f (fx, fy) (genImage (mandel_map fm px py (fromIntegral fx) (fromIntegral fy) fz) fm)


