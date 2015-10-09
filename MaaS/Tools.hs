module MaaS.Tools (clamp, remap) where

clamp :: Ord a => a -> a -> a -> a
clamp mn mx v = (max mn . min mx) v

remap :: RealFrac a => a -> a -> a -> a -> a -> a
remap x imin imax omin omax = (x-imin)*(omax-omin)/(imax-imin)+omin
