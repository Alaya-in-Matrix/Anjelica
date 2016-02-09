module Language.ElementaryArithmetic(
    exprParser
    , interpFile
    , interpString
    )where
-- Author: lvwenlong_lambda@qq.com
-- Last Modified:2016年02月09日 星期二 19时09分51秒 二
import           Control.Applicative           hiding ((<|>))
import           Data.String
import           Text.ParserCombinators.Parsec


data Expr  = Add Expr Expr2  | Sub Expr Expr2  | E2 Expr2
data Expr2 = Mul Expr2 Expr3 | Div Expr2 Expr3 | E3 Expr3
data Expr3 = Quote Expr      | NumLit Int

class Valuable a where
    eval :: a -> Int
instance Valuable Expr where
    eval (Add e1 e2) = eval e1 + eval e2
    eval (Sub e1 e2) = eval e1 - eval e2
    eval (E2 e)      = eval e
instance Valuable Expr2 where
    eval (Mul e1 e2) = eval e1 * eval e2
    eval (Div e1 e2) = eval e1 `quot` eval e2
    eval (E3 e)      = eval e
instance Valuable Expr3 where
    eval (Quote e)   = eval e
    eval (NumLit n)  = n

instance Show Expr where
    show (Add e1 e2) = "(+ " ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Sub e1 e2) = "(- " ++ show e1 ++ " " ++ show e2 ++ ")"
    show (E2 e)      = show e
instance Show Expr2 where
    show (Mul e1 e2) = "(* " ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Div e1 e2) = "(/ " ++ show e1 ++ " " ++ show e2 ++ ")"
    show (E3 e)      = show e
instance Show Expr3 where
    show (NumLit e)  = show e
    show (Quote e)   = show e

exprParser :: Parser Expr
exprParser = chainl1 (E2 <$> expr2Parser) exprOp
    where exprOp = do op <- Add <$ char '+' <|> Sub <$ char '-' <?> "Addition(+) or subtraction(-)"
                      return $ \e1 (E2 e2) -> op e1 e2

expr2Parser :: Parser Expr2
expr2Parser = chainl1 (E3 <$> expr3Parser) expr2Op
    where expr2Op = do op <- Mul <$ char '*' <|> Div <$ char '/' <?> "Multiplication(*) or division(/)"
                       return $ \e1 (E3 e2) -> op e1 e2
expr3Parser :: Parser Expr3
expr3Parser = quoteParser <|> literalParser <?> "Quoted expresion or number literal"
  where quoteParser   = Quote  <$> between (char '(') (char ')') exprParser
        literalParser = NumLit <$> read <$> many1 digit

interpString :: String -> IO ()
interpString  exprStr = let expr = filter (/= ' ') exprStr
                         in case parse (exprParser <* eof) "Elementary Arithmetic" expr of
                              Left err     -> error $ show err
                              Right parsed -> print parsed
                                              >> (print $ eval parsed)

interpFile :: String -> IO ()
interpFile file = readFile file
              >>= interpString
