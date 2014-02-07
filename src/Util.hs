module Util where

import Control.Monad

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond comp = do
    c <- cond
    when c comp

condM :: Monad m => m Bool -> m a -> m a -> m a
condM cond t f = do
    c <- cond
    if c then t else f
