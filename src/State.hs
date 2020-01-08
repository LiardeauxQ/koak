
module State
    ( State(..)
    , state
    , get
    , put
    , modify
    )
where

import           Control.Monad
import           Control.Applicative

newtype State s m a = State { runState :: s -> m (a, s) }

instance (Monad m) => Functor (State s m) where
    fmap = Control.Monad.liftM

instance (Monad m) => Applicative (State s m) where
    pure  = return
    (<*>) = Control.Monad.ap

instance (Monad m) => Monad (State s m) where
    return a = State $ \s -> return (a, s)
    m >>= k = State $ \s -> do
        (a, s') <- runState m s
        runState (k a) s'

instance (Alternative m, Monad m) => Alternative (State s m) where
    empty = State $ const empty
    (State a) <|> (State e) = State $ \s -> a s <|> e s

state :: (Monad m) => (s -> (a, s)) -> State s m a
state f = do
    s <- get
    let ~(a, s') = f s
    put s'
    return a

get :: (Monad m) => State s m s
get = State $ \s -> return (s, s)

put :: (Monad m) => s -> State s m ()
put s = State $ \_ -> return ((), s)

modify :: (Monad m) => (s -> s) -> State s m ()
modify f = state (\s -> ((), f s))
