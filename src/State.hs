module State
    ( StateT(..)
    , state
    , get
    , gets
    , put
    , modify
    )
where

import           Control.Monad
import           Control.Applicative

newtype StateT s m a = StateT { runState :: s -> m (a, s) }

instance (Monad m) => Functor (StateT s m) where
    fmap = Control.Monad.liftM

instance (Monad m) => Applicative (StateT s m) where
    pure  = return
    (<*>) = Control.Monad.ap

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    m >>= k = StateT $ \s -> do
        (a, s') <- runState m s
        runState (k a) s'

instance (Alternative m, Monad m) => Alternative (StateT s m) where
    empty = StateT $ const empty
    (StateT a) <|> (StateT e) = StateT $ \s -> a s <|> e s

state :: (Monad m) => (s -> (a, s)) -> StateT s m a
state f = do
    s <- get
    let ~(a, s') = f s
    put s'
    return a

get :: (Monad m) => StateT s m s
get = StateT $ \s -> return (s, s)

gets :: (Monad m) => (s -> a) -> StateT s m a
gets f = f <$> get

put :: (Monad m) => s -> StateT s m ()
put s = StateT $ \_ -> return ((), s)

modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = state (\s -> ((), f s))
