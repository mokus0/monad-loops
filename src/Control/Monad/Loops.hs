{-# LANGUAGE CPP #-}

-- |A collection of loop operators for use in monads (mostly in stateful ones).
-- 
-- There is a general naming pattern for many of these:
-- Functions with names ending in _ discard the results of the loop body
-- as in the standard Prelude 'mapM' functions.
-- 
-- Functions with names ending in ' collect their results into 'MonadPlus'
-- containers.  Note that any short-circuit effect that those types' 
-- 'MonadPlus' instances may provide in a lazy context (such as the instance
-- for 'Maybe') will _not_ cause execution to short-circuit in these loops.
--
-- Functions with names ending in neither of those will generally return
-- just plain old lists.

module Control.Monad.Loops
        ( module Control.Monad.Loops
        ) where

import Control.Monad

import Control.Exception
import Control.Concurrent

#ifndef base4
#define SomeException Exception
#endif

-- possibly-useful addition? :
-- concatMapM :: (Monad m, Traversable f, Monoid w) => (a -> m w) -> (f a) -> m w

-- would also like to implement an "interleavable" version of forkMapM (probably
-- using something other than a list in the return) that can effectively handle
-- very large or even infinite input lists.

-- |Like 'mapM', but run all the actions in parallel threads, collecting up
-- the results and returning them all.  Does not return until all actions finish.
forkMapM :: (a -> IO b) -> [a] -> IO [Either SomeException b]
forkMapM f xs = do
        mvars <- forM xs $ \x -> do
                mvar <- newEmptyMVar
                forkIO $ do
                        result <- handle (return . Left) $ do
                                y <- f x
                                return (Right y)
                        putMVar mvar result
                return mvar
        
        mapM takeMVar mvars

-- | like 'forkMapM' but without bothering to keep the return values
forkMapM_ :: (a -> IO b) -> [a] -> IO [Maybe SomeException]
forkMapM_ f xs = do
        mvars <- forM xs $ \x -> do
                mvar <- newEmptyMVar
                forkIO $ do
                        -- in base >=4, need to nail down the type of 'handle'
                        let handleAny :: (SomeException -> IO a) -> IO a -> IO a
                            handleAny = handle
                        result <- handleAny (return . Just) $ do
                                f x
                                return Nothing
                        putMVar mvar result
                return mvar
        
        mapM takeMVar mvars

-- | like 'forkMapM_' but not even bothering to track success or failure
-- of the child threads.  Still waits for them all though.
forkMapM__ :: (a -> IO b) -> [a] -> IO ()
forkMapM__ f xs = do
        mvars <- forM xs $ \x -> do
                mvar <- newEmptyMVar
                forkIO $ do
                        -- in base >=4, need to nail down the type of 'handle'
                        let handleAny :: (SomeException -> IO a) -> IO a -> IO a
                            handleAny = handle
                        handleAny (\_ -> return ()) $ do
                                f x
                                return ()
                        putMVar mvar ()
                return mvar
        
        mapM_ takeMVar mvars

{-# SPECIALIZE whileM  :: IO Bool -> IO a -> IO [a] #-}
{-# SPECIALIZE whileM' :: Monad m => m Bool -> m a -> m [a] #-}
{-# SPECIALIZE whileM' :: IO Bool -> IO a -> IO [a] #-}
{-# SPECIALIZE whileM_ :: IO Bool -> IO a -> IO () #-}

-- |Execute an action repeatedly as long as the given boolean expression
-- returns True.  The condition is evaluated before the loop body.
-- Collects the results into a list.
whileM :: Monad m => m Bool -> m a -> m [a]
whileM = whileM'

-- |Execute an action repeatedly as long as the given boolean expression
-- returns True. The condition is evaluated before the loop body.
-- Collects the results into an arbitrary 'MonadPlus' value.
whileM' :: (Monad m, MonadPlus f) => m Bool -> m a -> m (f a)
whileM' p f = go
    where go = do
            x <- p
            if x
                then do
                        x  <- f
                        xs <- go
                        return (return x `mplus` xs)
                else return mzero

-- |Execute an action repeatedly as long as the given boolean expression
-- returns True.  The condition is evaluated before the loop body.
-- Discards results.
whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
    where go = do
            x <- p
            if x
                then f >> go
                else return ()

-- |Execute an action repeatedly until its result fails to satisfy a predicate,
-- and return that result (discarding all others).
iterateWhile :: Monad m => (a -> Bool) -> m a -> m a
iterateWhile p = iterateUntil (not . p)

{-# SPECIALIZE iterateM_ :: (a -> IO a) -> a -> IO b #-}
-- |Execute an action forever, feeding the result of each execution as the
-- input to the next.
iterateM_ :: Monad m => (a -> m a) -> a -> m b
iterateM_ f = g
    where g x = f x >>= g

{-# SPECIALIZE untilM  :: IO a -> IO Bool -> IO [a] #-}
{-# SPECIALIZE untilM' :: Monad m => m a -> m Bool -> m [a] #-}
{-# SPECIALIZE untilM' :: IO a -> IO Bool -> IO [a] #-}
{-# SPECIALIZE untilM_ :: IO a -> IO Bool -> IO () #-}

infixr 0 `untilM`
infixr 0 `untilM'`
infixr 0 `untilM_`
infixr 0 `iterateUntilM`

-- |Execute an action repeatedly until the condition expression returns True.
-- The condition is evaluated after the loop body.  Collects results into a list.
-- Parameters are arranged for infix usage.  eg. do {...} `untilM_` ...
untilM :: Monad m => m a -> m Bool -> m [a]
untilM = untilM'

-- |Execute an action repeatedly until the condition expression returns True.
-- The condition is evaluated after the loop body.  Collects results into a
-- "MonadPlus" value.
-- Parameters are arranged for infix usage.  eg. do {...} `untilM_` ...
untilM' :: (Monad m, MonadPlus f) => m a -> m Bool -> m (f a)
f `untilM'` p = do
        x  <- f
        xs <- whileM' (liftM not p) f
        return (return x `mplus` xs)

-- |Execute an action repeatedly until the condition expression returns True.
-- The condition is evaluated after the loop body.  Discards results.
-- Parameters are arranged for infix usage.  eg. do {...} `untilM_` ...
untilM_ :: (Monad m) => m a -> m Bool -> m ()
f `untilM_` p = f >> whileM_ (liftM not p) f


-- | Analogue of @('Prelude.until')@
-- Yields the result of applying f until p holds.
iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f v 
    | p v       = return v
    | otherwise = f v >>= iterateUntilM p f

-- |Execute an action repeatedly until its result satisfies a predicate,
-- and return that result (discarding all others).
iterateUntil :: Monad m => (a -> Bool) -> m a -> m a
iterateUntil p x = x >>= iterateUntilM p (const x)

{-# SPECIALIZE whileJust  :: IO (Maybe a) -> (a -> IO b) -> IO [b] #-}
{-# SPECIALIZE whileJust' :: Monad m => m (Maybe a) -> (a -> m b) -> m [b] #-}
{-# SPECIALIZE whileJust' :: IO (Maybe a) -> (a -> IO b) -> IO [b] #-}
{-# SPECIALIZE whileJust_ :: IO (Maybe a) -> (a -> IO b) -> IO () #-}

-- |As long as the supplied "Maybe" expression returns "Just _", the loop
-- body will be called and passed the value contained in the 'Just'.  Results
-- are collected into a list.
whileJust :: Monad m => m (Maybe a) -> (a -> m b) -> m [b]
whileJust = whileJust'

-- |As long as the supplied "Maybe" expression returns "Just _", the loop
-- body will be called and passed the value contained in the 'Just'.  Results
-- are collected into an arbitrary MonadPlus container.
whileJust' :: (Monad m, MonadPlus f) => m (Maybe a) -> (a -> m b) -> m (f b)
whileJust' p f = go
    where go = do
            x <- p
            case x of
                Nothing -> return mzero
                Just x  -> do
                        x  <- f x
                        xs <- go
                        return (return x `mplus` xs)

-- |As long as the supplied "Maybe" expression returns "Just _", the loop
-- body will be called and passed the value contained in the 'Just'.  Results
-- are discarded.
whileJust_ :: (Monad m) => m (Maybe a) -> (a -> m b) -> m ()
whileJust_ p f = go
    where go = do
            x <- p
            case x of
                Nothing -> return ()
                Just x  -> do
                        f x
                        go

-- |Run the supplied "Maybe" computation repeatedly until it returns a
-- value.  Returns that value.
untilJust :: Monad m => m (Maybe a) -> m a
untilJust m = go
    where
        go = do
            x <- m
            case x of
                Nothing -> go
                Just x  -> return x

{-# SPECIALIZE unfoldM  :: IO (Maybe a) -> IO [a] #-}
{-# SPECIALIZE unfoldM' :: (Monad m) => m (Maybe a) -> m [a] #-}
{-# SPECIALIZE unfoldM' :: IO (Maybe a) -> IO [a] #-}
{-# SPECIALIZE unfoldM_ :: IO (Maybe a) -> IO () #-}

-- |The supplied "Maybe" expression will be repeatedly called until it
-- returns 'Nothing'.  All values returned are collected into a list.
unfoldM :: (Monad m) => m (Maybe a) -> m [a]
unfoldM = unfoldM'

-- |The supplied "Maybe" expression will be repeatedly called until it
-- returns 'Nothing'.  All values returned are collected into an arbitrary
-- 'MonadPlus' thing.
unfoldM' :: (Monad m, MonadPlus f) => m (Maybe a) -> m (f a)
unfoldM' m = whileJust' m return

-- |The supplied "Maybe" expression will be repeatedly called until it
-- returns 'Nothing'.  All values returned are discarded.
unfoldM_ :: (Monad m) => m (Maybe a) -> m ()
unfoldM_ m = whileJust_ m return

-- |Repeatedly evaluates the second argument until the value satisfies
-- the given predicate, and returns a list of all values that satisfied the
-- predicate.  Discards the final one (which failed the predicate).
unfoldWhileM :: Monad m => (a -> Bool) -> m a -> m [a]
unfoldWhileM p m = loop id
    where
        loop f = do
            x <- m
            if p x
                then loop (f . (x:))
                else return (f [])

-- |Repeatedly evaluates the second argument until the value satisfies
-- the given predicate, and returns a 'MonadPlus' collection of all values
-- that satisfied the predicate.  Discards the final one (which failed the predicate).
unfoldWhileM' :: (Monad m, MonadPlus f) => (a -> Bool) -> m a -> m (f a)
unfoldWhileM' p m = loop mzero
    where
        loop xs = do
            x <- m
            if p x
                then loop (xs `mplus` return x)
                else return xs

{-# SPECIALIZE unfoldrM  :: (a -> IO (Maybe (b,a))) -> a -> IO [b] #-}
{-# SPECIALIZE unfoldrM' :: (Monad m) => (a -> m (Maybe (b,a))) -> a -> m [b] #-}
{-# SPECIALIZE unfoldrM' :: (a -> IO (Maybe (b,a))) -> a -> IO [b] #-}

-- |See 'Data.List.unfoldr'.  This is a monad-friendly version of that.
unfoldrM :: (Monad m) => (a -> m (Maybe (b,a))) -> a -> m [b]
unfoldrM = unfoldrM'

-- |See 'Data.List.unfoldr'.  This is a monad-friendly version of that, with a
-- twist.  Rather than returning a list, it returns any MonadPlus type of your
-- choice.
unfoldrM' :: (Monad m, MonadPlus f) => (a -> m (Maybe (b,a))) -> a -> m (f b)
unfoldrM' f = go
    where go z = do
            x <- f z
            case x of
                Nothing         -> return mzero
                Just (x, z')    -> do
                        xs <- go z'
                        return (return x `mplus` xs)

{-# SPECIALIZE concatM :: [a -> IO a] -> (a -> IO a) #-}

-- |Compose a list of monadic actions into one action.  Composes using
-- ('>=>') - that is, the output of each action is fed to the input of
-- the one after it in the list.
concatM :: (Monad m) => [a -> m a] -> (a -> m a)
concatM fs = foldr (>=>) return fs

{-# SPECIALIZE andM :: [IO Bool] -> IO Bool #-}
{-# SPECIALIZE orM  :: [IO Bool] -> IO Bool #-}

-- |short-circuit 'and' for values of type Monad m => m Bool
andM :: (Monad m) => [m Bool] -> m Bool
andM []         = return True
andM (p:ps)     = do
        q <- p
        if q
                then andM ps
                else return False

-- |short-circuit 'or' for values of type Monad m => m Bool
orM :: (Monad m) => [m Bool] -> m Bool
orM []          = return False
orM (p:ps)      = do
        q <- p
        if q
                then return True
                else orM ps

{-# SPECIALIZE anyPM :: [a -> IO Bool] -> (a -> IO Bool) #-}
{-# SPECIALIZE allPM :: [a -> IO Bool] -> (a -> IO Bool) #-}

-- |short-circuit 'any' with a list of \"monadic predicates\".  Tests the
-- value presented against each predicate in turn until one passes, then
-- returns True without any further processing.  If none passes, returns False.
anyPM :: (Monad m) => [a -> m Bool] -> (a -> m Bool)
anyPM []     _ = return False
anyPM (p:ps) x = do
        q <- p x
        if q
                then return True
                else anyPM ps x

-- |short-circuit 'all' with a list of \"monadic predicates\".  Tests the value
-- presented against each predicate in turn until one fails, then returns False.
-- if none fail, returns True.
allPM :: (Monad m) => [a -> m Bool] -> (a -> m Bool)
allPM []     _ = return True
allPM (p:ps) x = do
        q <- p x
        if q
                then allPM ps x
                else return False

{-# SPECIALIZE anyM :: (a -> IO Bool) -> [a] -> IO Bool #-}
{-# SPECIALIZE allM :: (a -> IO Bool) -> [a] -> IO Bool #-}

-- |short-circuit 'any' with a \"monadic predicate\".
anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM _ []       = return False
anyM p (x:xs)   = do
        q <- p x
        if q
                then return True
                else anyM p xs

-- |short-circuit 'all' with a \"monadic predicate\".
allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ []       = return True
allM p (x:xs)   = do
        q <- p x
        if q
                then allM p xs
                else return False

-- | Monadic 'takeWhile'.
takeWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
takeWhileM _ []     = return []
takeWhileM p (x:xs) = do
        q <- p x
        if q
                then (takeWhileM p xs) >>= (return . (:) x)
                else return []

-- | Monadic 'dropWhile'.
dropWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
dropWhileM _ []     = return []
dropWhileM p (x:xs) = do
        q <- p x
        if q
                then dropWhileM p xs
                else return (x:xs)

-- |like 'dropWhileM' but trims both ends of the list.
trimM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
trimM p xs = do
        xs <- dropWhileM p xs
        rxs <- dropWhileM p (reverse xs)
        return (reverse rxs)

-- |return the first value from a list, if any, satisfying the given predicate.
firstM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
firstM _ [] = return Nothing
firstM p (x:xs) = do
        q <- p x
        if q
                then return (Just x)
                else firstM p xs

{-# INLINE minimaOnByM #-}
minimaOnByM :: Monad m => (a -> m b) -> (b -> b -> m Ordering) -> [a] -> m [a]
minimaOnByM _   _ []     = return []
minimaOnByM f cmp (x:xs) = do
    fx<- f x
    loop (x:) fx xs
        where   loop ms _ []           = return (ms [])
                loop ms fm (x:xs) = do
                    fx  <- f x
                    ord <- cmp fm fx
                    case ord of
                        LT -> loop ms          fm xs
                        EQ -> loop (ms . (x:)) fm xs
                        GT -> loop (x:)        fx xs

{-# INLINE maximaOnByM #-}
maximaOnByM :: Monad m => (a -> m b) -> (b -> b -> m Ordering) -> [a] -> m [a]
maximaOnByM f = minimaOnByM f . flip

minimaByM :: Monad m => (a -> a -> m Ordering) -> [a] -> m [a]
minimaByM = minimaOnByM return

maximaByM :: Monad m => (a -> a -> m Ordering) -> [a] -> m [a]
maximaByM = maximaOnByM return

minimaOnM :: (Monad m, Ord b) => (a -> m b) -> [a] -> m [a]
minimaOnM f = minimaOnByM f (\x y -> return (compare x y))

maximaOnM :: (Monad m, Ord b) => (a -> m b) -> [a] -> m [a]
maximaOnM f = maximaOnByM f (\x y -> return (compare x y))

{-# INLINE minimumOnByM #-}
minimumOnByM :: Monad m => (a -> m b) -> (b -> b -> m Ordering) -> [a] -> m (Maybe a)
minimumOnByM _   _ []     = return Nothing
minimumOnByM f cmp (x:xs) = do
    fx <- f x
    loop x fx xs
        where   loop m _ []           = return (Just m)
                loop m fm (x:xs) = do
                    fx  <- f x
                    ord <- cmp fm fx
                    case ord of
                        LT -> loop m fm xs
                        EQ -> loop m fm xs
                        GT -> loop x fx xs

{-# INLINE maximumOnByM #-}
maximumOnByM :: Monad m => (a -> m b) -> (b -> b -> m Ordering) -> [a] -> m (Maybe a)
maximumOnByM f = minimumOnByM f . flip

minimumByM :: Monad m => (a -> a -> m Ordering) -> [a] -> m (Maybe a)
minimumByM = minimumOnByM return

maximumByM :: Monad m => (a -> a -> m Ordering) -> [a] -> m (Maybe a)
maximumByM = maximumOnByM return

minimumOnM :: (Monad m, Ord b) => (a -> m b) -> [a] -> m (Maybe a)
minimumOnM f = minimumOnByM f (\x y -> return (compare x y))

maximumOnM :: (Monad m, Ord b) => (a -> m b) -> [a] -> m (Maybe a)
maximumOnM f = maximumOnByM f (\x y -> return (compare x y))
