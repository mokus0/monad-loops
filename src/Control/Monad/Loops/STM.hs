{-
 -      ``Control/Monad/Loops/STM''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Control.Monad.Loops.STM where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad (forever) -- for the benefit of haddock

-- |'Control.Monad.forever' and 'Control.Concurrent.STM.atomically' rolled
-- into one.
atomLoop :: STM a -> IO ()
atomLoop x = atomically x >> atomLoop x

-- |'atomLoop' with a 'forkIO'
forkAtomLoop :: STM a -> IO ThreadId
forkAtomLoop = forkIO . atomLoop

-- |'Control.Concurrent.STM.retry' until the given condition is true of
-- the given value.  Then return the value that satisfied the condition.
waitFor :: (a -> Bool) -> STM a -> STM a
waitFor p events = do
        event <- events
        if p event
                then return event
                else retry

-- |'Control.Concurrent.STM.retry' until the given value is True.
waitForTrue :: STM Bool -> STM ()
waitForTrue p = waitFor id p >> return ()

-- |'waitFor' a value satisfying a condition to come out of a
-- 'Control.Concurrent.STM.TChan', reading and discarding everything else.
-- Returns the winner.
waitForEvent :: (a -> Bool) -> TChan a -> STM a
waitForEvent p events = waitFor p (readTChan events)
