module Control.Cumae () where

import Data.Monoid
import Control.Arrow
import Control.Applicative

data Tree' p w = Tree' p (Forest' p w)

data Forest' p w = Forest' w [Tree' p w] w

data Sibyl p w m a b = Sibyl (Forest' p w) (a -> m b)

foretell :: Monoid w => Sibyl p w m a b -> p -> Sibyl p w m a b
foretell (Sibyl ts k) r = Sibyl (Forest' mempty [Tree' r ts] mempty) k

preface :: Monoid w => Sibyl p w m a b -> w -> Sibyl p w m a b
preface (Sibyl (Forest' w1 p w2) k) w = Sibyl (Forest' (w <> w1) p w2) k

coda :: Monoid w => Sibyl p w m a b -> w -> Sibyl p w m a b
coda (Sibyl (Forest' w1 p w2) k) w = Sibyl (Forest' w1 p (w2 <> w)) k

-- tracker-arrow
-- Control.Arrow.Tracker
-- step
-- before
-- after
