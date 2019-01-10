module Traze.Internal.Tuple where

tupleDo :: (a -> b) -> (a, a) -> (b, b)
tupleDo f (x, y) = (f x, f y)

tupleFold :: (a -> b -> c) -> (a, b) -> c
tupleFold f (x, y) = f x y

tupleMap :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
tupleMap f (x1, y1) (x2, y2) = (x1 `f` x2, y1 `f` y2)

tupleApply :: (a -> c, b -> d) -> (a, b) -> (c, d)
tupleApply (f, g) (a, b) = (f a, g b)

