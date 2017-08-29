module State ( 
    State(..),
    put,
    modify
) where

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f fs = State $ \s ->
        let (a, s2) = runState fs s
        in (f a, s2)

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    f <*> fa = State $ \s ->
        let (fab, s0) = runState f s
            (a, s1) = runState fa s0
            in (fab a, s1)

instance Monad (State s) where
    return = pure
    fa >>= f = State $ \s ->
        let (a, s2) = runState fa s
        in runState (f a) s2

put s = State $ \_ -> ((), s)
modify f = State $ \s -> ((), f s)