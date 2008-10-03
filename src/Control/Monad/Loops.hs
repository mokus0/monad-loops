{-
 -      ``Control/Monad/Loops''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        CPP
  #-}

module Control.Monad.Loops
        ( module Control.Monad.Loops
#ifdef useSTM
        , module Control.Monad.Loops.STM
#endif
        ) where

import Control.Monad

#ifdef useSTM
import Control.Monad.Loops.STM
#endif

{-# SPECIALIZE while :: IO Bool -> IO a -> IO () #-}

while :: (Monad m) => m Bool -> m a -> m ()
while p f = do
        x <- p
        if x
                then do
                        f
                        while p f
                else return ()

{-# SPECIALIZE whileJust :: IO (Maybe a) -> (a -> IO b) -> IO () #-}

whileJust :: (Monad m) => m (Maybe a) -> (a -> m b) -> m ()
whileJust p f = do
        x <- p
        case x of
                Nothing -> return ()
                Just x  -> do
                        f x
                        whileJust p f

{-# SPECIALIZE unfoldM :: (Monad m) => m (Maybe a) -> m [a] #-}
{-# SPECIALIZE unfoldM :: IO (Maybe a) -> IO [a] #-}

unfoldM :: (Monad m, MonadPlus f) => m (Maybe a) -> m (f a)
unfoldM m = do
        x <- m
        case x of
                Nothing -> return mzero
                Just x  -> do
                        xs <- unfoldM m
                        return (return x `mplus` xs)

{-# SPECIALIZE unfoldrM :: (Monad m) => (a -> m (Maybe (b,a))) -> a -> m [b] #-}
{-# SPECIALIZE unfoldrM :: (a -> IO (Maybe (b,a))) -> a -> IO [b] #-}

unfoldrM :: (Monad m, MonadPlus f) => (a -> m (Maybe (b,a))) -> a -> m (f b)
unfoldrM f z = do
        x <- f z
        case x of
                Nothing         -> return mzero
                Just (x, z)     -> do
                        xs <- unfoldrM f z
                        return (return x `mplus` xs)

{-# SPECIALIZE concatM :: [a -> IO a] -> (a -> IO a) #-}

concatM :: (Monad m) => [a -> m a] -> (a -> m a)
concatM fs x = foldl (>>=) (return x) fs

{-# SPECIALIZE anyM :: [a -> IO Bool] -> (a -> IO Bool) #-}
{-# SPECIALIZE allM :: [a -> IO Bool] -> (a -> IO Bool) #-}

anyM, allM :: (Monad m) => [a -> m Bool] -> (a -> m Bool)
anyM []     x = return False
anyM (p:ps) x = do
        q <- p x
        if q
                then return True
                else anyM ps x

allM []     x = return True
allM (p:ps) x = do
        q <- p x
        if q
                then allM ps x
                else return False

dropWhileM, trimM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
dropWhileM p []     = return []
dropWhileM p (x:xs) = do
        q <- p x
        if q
                then dropWhileM p xs
                else return xs

trimM p xs = do
        xs <- dropWhileM p xs
        rxs <- dropWhileM p (reverse xs)
        return (reverse rxs)

firstM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
firstM p [] = return Nothing
firstM p (x:xs) = do
        q <- p x
        if q
                then return (Just x)
                else firstM p xs
