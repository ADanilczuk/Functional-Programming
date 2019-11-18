import Control.Monad       (liftM, ap)
import Control.Monad.State

data Act = Tick|Display|SetTime Int
         deriving (Eq, Show)

runClock :: [Act] -> [Int]
runClock acts = fst(runC acts 0)

runC :: [Act] -> Int -> ([Int],Int)
runC (Tick:acts) t = let (displays, time)= runC acts (t+1) 
                     in (displays, time)
runC (Display:acts) t = let (displays, time)=runC acts t 
                        in (t:displays, time) 
runC ((SetTime nt):acts) t = let (displays, time)= runC acts nt 
                             in (displays, time)
runC [] t = ([], t) 

ctest = runClock [Display,Tick,Tick,Display,Tick,SetTime 5,Tick,Display]

type Clock a = State Int a  

mrunClock :: [Act] -> [Int]        
mrunClock acts = evalState (mrunC acts) 0 
                         -- fst (runState (mrunC acts) 0) 

mrunC :: [Act] -> Clock [Int]
mrunC (Tick:acts) = do t <- get
                       put (t+1)
                       displays <- mrunC acts
                       return displays
mrunC (Display:acts) = do  t <- get
                           displays <- mrunC acts
                           return (t:displays)                       
mrunC ((SetTime nt):acts) = do put nt
                               displays <- mrunC acts
                               return displays                               
mrunC [] = return []

mctest = mrunClock [Display,Tick,Tick,Display,Tick,SetTime 5,Tick,Display]