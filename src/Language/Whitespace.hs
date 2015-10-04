module Language.Whitespace where

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative hiding((<|>), many)
import System.Environment
import Data.Maybe
import qualified Data.Map as M

data IMP     = Stack      StackOp
             | Arithmetic ArithOp
             | Heap       HeapOp
             | Control    CtrlCmd
             | IO         IOCmd deriving(Show, Eq)
data StackOp = Push Integer 
             | Duplicate 
             | Swap 
             | Pop deriving(Eq, Show)
data ArithOp = Add 
             | Sub 
             | Mul 
             | Div 
             | Mod deriving(Eq, Show)
data HeapOp  = Store | Retrieve deriving(Eq, Show)
data IOCmd   = OutChar 
             | OutInt 
             | ReadChar 
             | ReadInt deriving(Eq, Show)
data CtrlCmd = Label Integer 
             | Call Integer 
             | Jump Integer 
             | JumpZero Integer 
             | JumpNeg Integer 
             | FuncEnd 
             | ProgEnd deriving(Eq, Show)

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
      '\n' -> Duplicate <$ wsSpace 
              <|> Swap  <$ wsTab   
              <|> Pop   <$ wsLf

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

data VM = VM {
    stack :: [Integer] , 
    heap :: M.Map Integer Integer,
    instructions :: [IMP] , -- This should be an array(vector) instead of a list
    pc :: Int , 
    jumptable :: M.Map Integer Int
} deriving(Eq, Show)

eval :: VM -> IO VM
eval vm = let index   = pc vm
              codeMem = instructions vm
              instr   = codeMem !! index
           in case instr of
                Control ProgEnd -> return vm
                _               -> evalInstr vm instr >>= eval


evalInstr :: VM -> IMP -> IO VM
evalInstr (VM s h i p j) (Stack (Push num)) = return $ VM (num:s)  h i (p+1) j
evalInstr (VM s h i p j) (Stack Duplicate)  = undefined
evalInstr (VM s h i p j) (Stack Pop)        = return $ VM (tail s) h i (p+1) j
evalInstr (VM s h i p j) (Stack Swap)       = return $ VM (swap s) h i (p+1) j 
  where swap (a:b:rest) = (b:a:rest)
evalInstr (VM (x:y:rest) h i p j) (Arithmetic arOp) = return $ VM ((x `op` y):rest) h i (p+1) j 
  where op = case arOp of
               Add -> (+)
               Sub -> (-)
               Mul -> (*)
               Div -> div
               Mod -> mod
evalInstr (VM (val:addr:rest) h i p j) (Heap Store) = return $ VM rest newHeap i (p+1) j
  where newHeap = M.insert addr val h 
evalInstr (VM (addr:rest) h i p j) (Heap Retrieve)  = return $ VM (val:rest) h i (p+1) j
  where val = fromJust $ M.lookup addr h
evalInstr vm@(VM s h i p j) (IO OutChar)  = (VM s h i (p+1) j)<$ (putChar . toEnum . fromEnum . head) s
evalInstr vm@(VM s h i p j) (IO OutInt)   = (VM s h i (p+1) j)<$  (putStr  . show   . fromEnum . head) s
evalInstr vm@(VM s h i p j) (IO ReadChar) = do int <- (toInteger.fromEnum) <$> getChar
                                               return $ VM (int:s) h i (p+1) j
evalInstr vm@(VM s h i p j) (IO ReadInt)  = do int <- read <$> getLine
                                               return $ VM (int:s) h i (p+1) j
evalInstr (VM s h i p j) (Control (Label n)) = return $ VM s h i (p+1) newJump
  where newJump = M.insert n (p+1) j
evalInstr (VM s h i p j) (Control (Call label)) = undefined
evalInstr (VM s h i p j) (Control FuncEnd)      = undefined
evalInstr (VM s h i p j) (Control ProgEnd)      = undefined
evalInstr (VM s h i p j) (Control (Jump n)) = return $ VM s h i newPC j
  where newPC = fromJust $ M.lookup n j
evalInstr (VM s h i p j) (Control (JumpNeg n)) = return $ VM s h i newPC j
  where newPC = if ((head s) < 0) then fromJust $ M.lookup n j
                                  else p+1
evalInstr (VM s h i p j) (Control (JumpZero n)) = return $ VM s h i newPC j
  where newPC = if ((head s) == 0) then fromJust $ M.lookup n j
                                   else p+1


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
