module Main where
import Text.ParserCombinators.Parsec
import Control.Applicative

data MoveList a = MoveList {
    preList :: [a],
    currentList :: [a]
} deriving(Eq, Show)
instance Functor MoveList  where
    fmap f (MoveList prev curr) = MoveList (f <$> prev) (f <$> curr)

env :: MoveList Char
env = fmap toEnum $ MoveList [0..] [0..]

headApply :: (a->a) -> MoveList a -> MoveList a
headApply f (MoveList _ []) = error "empty!"
headApply f (MoveList prev (c:cs)) = MoveList prev ((f c) : cs)
next :: MoveList a -> MoveList a
next (MoveList pre (c:cs)) = MoveList (c:pre) cs
prev :: MoveList a -> MoveList a
prev (MoveList (p:ps) curr) = MoveList ps (p:curr)



addOne :: (Enum a) => MoveList a -> MoveList a
addOne = headApply succ

subOne :: (Enum a) => MoveList a -> MoveList a
subOne = headApply pred


main = undefined


