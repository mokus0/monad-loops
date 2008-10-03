{-
 -      ``Control/Monad/Loops/STM''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Control.Monad.Loops.STM where

import Control.Monad

import Control.Concurrent
import Control.Concurrent.STM

atomLoop :: STM a -> IO ()
atomLoop = forever . atomically

forkAtomLoop :: STM a -> IO ThreadId
forkAtomLoop = forkIO . atomLoop

waitFor :: (a -> Bool) -> STM a -> STM a
waitFor p events = do
        event <- events
        if p event
                then return event
                else retry

waitForTrue :: STM Bool -> STM ()
waitForTrue p = waitFor id p >> return ()

waitForEvent :: (a -> Bool) -> TChan a -> STM a
waitForEvent p events = waitFor p (readTChan events)
