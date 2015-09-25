module Main where
import Text.ParserCombinators.Parsec
import Control.Applicative hiding((<|>), many)
import Control.Monad
import System.Environment

-- both preList and currentList are supposed to be infinite list
-- so I won't consider the situation of empty list
data MoveList = MoveList {
    preList :: [Char],
    currentList :: [Char]
} deriving(Eq)

instance Show MoveList where 
    show (MoveList _ (c:cs)) = "MoveList [...] [" ++ [c] ++ ", ...]"

env :: MoveList 
env = MoveList init init where init = map toEnum [0..]

next      (MoveList pre    (c:cs)) = MoveList (c:pre) cs
prev      (MoveList (p:ps) curr)   = MoveList ps (p:curr)
headEle   (MoveList prev   curr)   = head curr

newHead   x (MoveList prev   (c:cs)) = MoveList prev (x:cs)
headApply f (MoveList pre (c:cs))    = MoveList pre ((f c) : cs)

addOne = headApply succ
subOne = headApply pred

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
                       <?> "Anjelica-Ebbi"

evalOne :: MoveList  -> BrainFuck -> IO MoveList
evalOne env RightMove    = return $ next   env
evalOne env LeftMove     = return $ prev   env
evalOne env Add          = return $ addOne env
evalOne env Sub          = return $ subOne env
evalOne env Output       = putChar (headEle env) >> return env
evalOne env Input        = getChar >>= return . (flip newHead) env
evalOne env (Loop exprs) = case fromEnum (headEle env) of
                             0 -> return env
                             _ -> do newEnv <- (foldM evalOne) env exprs
                                     evalOne newEnv (Loop exprs)

main :: IO ()
main = do 
    filePath <- head <$> getArgs
    putStrLn "Source"
    content  <- filter (`elem` "><+-.,[]") <$> readFile filePath
    putStrLn content
    putStrLn "============================="
    let parsedContent = parse parseBrainFuck "BrainFuck" content
    case parsedContent of
      Left  err   -> print err
      Right exprs -> do
          putStrLn "Result"
          foldM_ evalOne env exprs
