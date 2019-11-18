import Data.STRef
import Control.Monad.ST

swapST :: (Int,Int) -> (Int,Int)
swapST (x,y) = runST $ do
   xr <- newSTRef x
   yr <- newSTRef y
   temp <-  readSTRef xr
   yValue <- readSTRef yr
   writeSTRef xr yValue
   writeSTRef yr temp
   xfinal <- readSTRef xr
   yfinal <- readSTRef yr
   return (xfinal,yfinal)
   
