module Language.BrainFuck(
    interpString,
    interpFile,
    )where

import           Control.Applicative           hiding (many, (<|>))
import           Control.Monad
import           System.Environment
import           Text.ParserCombinators.Parsec


-- both preList and currentList are supposed to be infinite list
-- so I won't consider the situation of empty list
data MoveList = MoveList {
    preList     :: [Int],
    currentList :: [Int]
} deriving(Eq)


next      (MoveList pre    (c:cs))   = MoveList (c:pre) cs
prev      (MoveList (p:ps) curr)     = MoveList ps (p:curr)
headEle   (MoveList prev   curr)     = head curr
newHead   x (MoveList prev   (c:cs)) = MoveList prev (x:cs)
headApply f (MoveList pre (c:cs))    = MoveList pre ((f c) : cs)
add n = headApply (+n)
sub n = headApply (subtract n)

instance Show MoveList where
    show (MoveList _ (c:cs)) = "MoveList [...] [" ++ [toEnum c] ++ ", ...]"

data BrainFuck = RightMove
               | LeftMove
               | Add Int
               | Sub Int
               | Output
               | Input
               | Loop [BrainFuck]
               deriving(Eq, Show)

parseBrainFuck :: Parser [BrainFuck]
parseBrainFuck = many parseBrainFuckOp
  where parseBrainFuckOp = RightMove <$ char '>'
                       <|> LeftMove  <$ char '<'
                       <|> Add <$> (length <$> (many1 $ char '+'))
                       <|> Sub <$> (length <$> (many1 $ char '-'))
                       <|> Output    <$ char '.'
                       <|> Input     <$ char ','
                       <|> Loop <$> between (char '[') (char ']') parseBrainFuck
                       <?> "Anjelica-Ebbi"


eval :: MoveList  -> BrainFuck -> IO MoveList
eval env RightMove    = return $ next   env
eval env LeftMove     = return $ prev   env
eval env (Add n)      = return $ add n  env
eval env (Sub n)      = return $ sub n  env
eval env Output       = putChar (toEnum $ headEle env) >> return env
eval env Input        = (fromEnum <$> getChar) >>= return . (flip newHead) env
eval env (Loop exprs) = case fromEnum (headEle env) of
                          0 -> return env
                          _ -> foldM eval env exprs >>= flip eval (Loop exprs)

interpString :: String -> IO ()
interpString bfCode = do
    let content = filter (`elem` "><+-.,[]") bfCode
    putStrLn "BrainFuck Code:"
    putStrLn content
    putStrLn "======================================"
    let parsed = parse parseBrainFuck "BrainFuck" content
        env    = MoveList init init where init = repeat 0
    case parsed of
         Left err    -> print err
         Right exprs -> do putStrLn "Result: "
                           foldM_ eval env exprs

interpFile :: FilePath -> IO ()
interpFile file = readFile file >>= interpString
