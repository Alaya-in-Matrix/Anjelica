module Main where
import Text.ParserCombinators.Parsec
import Control.Applicative hiding((<|>), many)
import Control.Monad
import System.Environment

-- both preList and currentList are supposed to be infinite list
-- so I won't consider the situation of empty list
data MoveList a = MoveList {
    preList :: [a],
    currentList :: [a]
} deriving(Eq)

instance (Enum a) => Show (MoveList a) where 
    show (MoveList _ (c:cs)) = let showChar = toEnum $ fromEnum c
                                in "MoveList [...] [" ++ [showChar] ++ ", ...]"
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
headEle :: (MoveList a) -> a
headEle (MoveList prev curr) = head curr
newHead :: a -> (MoveList a) -> MoveList a
newHead x (MoveList prev (c:cs)) = MoveList prev (x:cs)
data BrainFuck = RightMove 
               | LeftMove
               | Add
               | Sub
               | Output
               | Input
               | Loop [BrainFuck]
               deriving(Eq, Show)

parseBrainFuck :: Parser [BrainFuck]
parseBrainFuck = many parseBrainFuckOp
  where parseBrainFuckOp = RightMove <$ char '>'
                       <|> LeftMove  <$ char '<'
                       <|> Add       <$ char '+'
                       <|> Sub       <$ char '-'
                       <|> Output    <$ char '.'
                       <|> Input     <$ char ','
                       <|> Loop <$> between (char '[') (char ']') parseBrainFuck
                       <?> "BRAINFUCK EXPRESSION"
-- parseBrainFuck = many $ RightMove <$ char '>'
--                     <|> LeftMove  <$ char '<'
--                     <|> Add       <$ char '+'
--                     <|> Sub       <$ char '-'
--                     <|> Output    <$ char '.'
--                     <|> Input     <$ char ','
--                     <|> Loop <$> between (char '[') (char ']') parseBrainFuck
--                     <?> "BRAINFUCK EXPRESSION"


evalOne :: (Enum a) => (MoveList a) -> BrainFuck -> IO (MoveList a)
evalOne env RightMove = return $ next env
evalOne env LeftMove  = return $ prev env
evalOne env Add       = return $ addOne env
evalOne env Sub       = return $ subOne env
evalOne env Output    = do
    putChar $ toEnum $ fromEnum $ headEle env
    return env
evalOne env Input     = do
    newChar <- getChar
    return $ newHead (toEnum $ fromEnum newChar) env
evalOne env (Loop exprs) = do
    let ptrVal = fromEnum $ headEle env
    if ptrVal == 0
       then return env
       else do
           newEnv <- eval env exprs
           evalOne newEnv (Loop exprs)
eval :: (Enum a) => (MoveList a) -> [BrainFuck] -> IO (MoveList a)
eval  = foldM evalOne 
main :: IO ()
main = do 
    filePath <- head <$> getArgs
    parsedContent <- parseFromFile parseBrainFuck filePath
    case parsedContent of
      Left err    -> print err
      Right exprs -> do
          eval env exprs
          return ()


