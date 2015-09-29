module Language.Whitespace where

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative hiding((<|>), many)
import System.Environment

data IMP = Stack      StackOp
         | Arithmetic ArithOp
         | Heap       HeapOp
         | Control    CtrlCmd
         | IO         IOCmd deriving(Show, Eq)

data StackOp = Push Integer | Top  | Copy Integer | Swap | Pop | Slide Integer deriving(Show, Eq)
data ArithOp = Add | Sub | Mul | Div | Mod deriving(Eq, Show)
data HeapOp  = Store | Retrieve deriving(Eq, Show)
data IOCmd   = OutChar | OutInt | ReadChar | ReadInt deriving(Eq, Show)
data CtrlCmd = Label Integer | Call Integer | Jump Integer | JumpZero Integer | JumpNeg Integer | FuncEnd | ProgEnd deriving(Eq, Show)

wsSpace = char ' '
wsTab   = char '\t'
wsLf    = char '\n'

impParser :: Parser IMP
impParser = wsLf *> (Control  <$> ctrlCmdParser)
        <|> wsSpace *> (Stack <$> stackOpParser)
        <|> wsTab   *> (wsSpace *> (Arithmetic <$> arithOpParser) <|> 
                        wsTab   *> (Heap <$> heapOpParser)        <|>
                        wsLf    *> (IO <$> ioCmdParser))

numParser :: Parser Integer
numParser = ((*) <$> signParser <*> intParser) <* wsLf
  where signParser    = 1 <$ wsSpace <|> (-1) <$ wsTab
        bitParser     = 0 <$ wsSpace <|> 1    <$ wsTab
        intParser     = bitsToInteger <$> (many bitParser)
        bitsToInteger = foldl ((+) . (*2)) 0

stackOpParser :: Parser StackOp 
stackOpParser = do
    firstChar <- oneOf " \t\n"
    case firstChar of
      ' '  -> Push <$> numParser
      '\t' -> ((Copy <$ wsSpace) <|> (Slide <$ wsLf)) <*> numParser
      '\n' -> Top  <$ wsSpace <|> 
              Swap <$ wsTab   <|> 
              Pop  <$ wsLf

arithOpParser :: Parser ArithOp
arithOpParser = wsSpace *> (Add <$ wsSpace <|> Sub <$ wsTab <|> Mul <$ wsLf)
            <|> wsTab   *> (Div <$ wsSpace <|> Mod <$ wsTab)

heapOpParser :: Parser HeapOp
heapOpParser = Store <$ wsSpace <|> Retrieve <$ wsTab

ioCmdParser :: Parser IOCmd
ioCmdParser = wsSpace *> (OutChar  <$ wsSpace <|> OutInt  <$ wsTab)
          <|> wsTab   *> (ReadChar <$ wsSpace <|> ReadInt <$ wsTab)

ctrlCmdParser :: Parser CtrlCmd
ctrlCmdParser = wsSpace *> (wsSpace *> (Label <$> numParser) <|>
                            wsTab   *> (Call  <$> numParser) <|>
                            wsLf    *> (Jump  <$> numParser))
            <|> wsTab   *> (wsSpace *> (JumpZero <$> numParser) <|>
                            wsTab   *> (JumpNeg  <$> numParser) <|>
                            FuncEnd <$ wsLf)
            <|> FuncEnd <$ (wsLf *> wsLf)
-- num   = "\t \t\t\n" -- (-3)
-- testInstructions = [
--     " "    ++ num 
--   , " "    ++ "\n " 
--   , " "    ++ "\t "  ++ num 
--   , " "    ++ "\n\t" 
--   , " "    ++ "\n\n" 
--   , " "    ++ "\t\n" ++ num

--   , "\t "  ++ "  "
--   , "\t "  ++ " \t"
--   , "\t "  ++ " \n"
--   , "\t "  ++ "\t "
--   , "\t "  ++ "\t\t"

--   , "\t\t" ++ "\t"
--   , "\t\t" ++ " "

--   , "\t\n" ++ " \t"
--   , "\t\n" ++ "  "
--   , "\t\n" ++ "\t\t"
--   , "\t\n" ++ "\t "

--   , "\n"   ++ "  "   ++ num
--   , "\n"   ++ " \t"  ++ num
--   , "\n"   ++ " \n"  ++ num
--   , "\n"   ++ "\t "  ++ num
--   , "\n"   ++ "\t\t" ++ num
--   , "\n"   ++ "\t\n"
--   , "\n"   ++ "\n\n"
--   ]
-- test :: IO ()
-- test = mapM_ (parseTest impParser) testInstructions
