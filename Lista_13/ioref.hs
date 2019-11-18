import Data.IORef
import Control.Monad

sumNumbers :: IO Int
sumNumbers = do
  s <- newIORef 0
  go s
  readIORef s
 where
   go s = do
     putStr "Enter next integer number (empty line to finish): "
     n <- getLine
     when (not $ null n) $ do
       let num = read n
       modifyIORef' s (+ num)
       go s

main = do
  s <- sumNumbers
  putStr "Your sum is: "
  print s

{-
-- | Conditional execution of 'Applicative' expressions. For example,
--
-- > when debug (putStrLn "Debugging")
--
-- will output the string @Debugging@ if the Boolean value @debug@
-- is 'True', and otherwise do nothing.
when      :: (Applicative f) => Bool -> f () -> f ()
{-# INLINABLE when #-}
{-# SPECIALISE when :: Bool -> IO () -> IO () #-}
{-# SPECIALISE when :: Bool -> Maybe () -> Maybe () #-}
when p s  = if p then s else pure ()
-}
  






