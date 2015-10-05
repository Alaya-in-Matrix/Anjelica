module Language.Whitespace where

-- Author: lvwenlong_lambda@qq.com
-- Last Modified:2015年10月05日 星期一 22时16分38秒 一

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative hiding((<|>), many)
import System.Environment
import Data.Maybe
import Numeric (showIntAtBase)
import Data.Char(intToDigit)
import qualified Data.Map.Strict as M

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

-- Perhaps I should test it through QuichCheck
-- Property: disAssembly (parseFunc str) == str
class DisAssembly a where 
    disAssembly :: a -> String
instance DisAssembly Integer where
    disAssembly n = let absn    = abs n
                        sEncode = if signum n == 1 then ' ' else '\t' -- space for positive and tab for negative
                        nEncode = map (\c->if c == '0' then ' ' else '\t') $ showIntAtBase 2 intToDigit absn ""
                     in sEncode : nEncode ++ "\n"
instance DisAssembly StackOp where
    disAssembly stackOp = case stackOp of 
                            (Push n)  -> ' ' : disAssembly n
                            Duplicate -> "\n "
                            Swap      -> "\n\t"
                            Pop       -> "\n\n"
instance DisAssembly ArithOp where
    disAssembly arithOp = case arithOp of
                            Add -> "  "
                            Sub -> " \t"
                            Mul -> " \n"
                            Div -> "\t "
                            Mod -> "\t\t"
instance DisAssembly HeapOp where
    disAssembly heapOp = case heapOp of
                           Store    -> " "
                           Retrieve -> "\t"
instance DisAssembly IOCmd where
    disAssembly ioCmd = case ioCmd of
                          OutChar  -> "  "
                          OutInt   -> " \t"
                          ReadChar -> "\t "
                          ReadInt  -> "\t\t"
instance DisAssembly CtrlCmd where
    disAssembly ctrlCmd = case ctrlCmd of
                            Label n    -> "  "  ++ disAssembly n
                            Call  n    -> " \t" ++ disAssembly n
                            Jump  n    -> " \n" ++ disAssembly n
                            JumpZero n -> "\t " ++  disAssembly n
                            JumpNeg n  -> "\t\t" ++ disAssembly n
                            FuncEnd    -> "\t\n"
                            ProgEnd    -> "\n\n"

instance DisAssembly IMP where
    disAssembly (Stack para)      = " "    ++ disAssembly para
    disAssembly (Arithmetic para) = "\t"   ++ disAssembly para
    disAssembly (Heap para)       = "\t\t" ++ disAssembly para
    disAssembly (IO para)         = "\t\n" ++ disAssembly para
    disAssembly (Control para)    = "\n"   ++ disAssembly para


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

initVM :: [IMP] -> VM
initVM instrs = let stack     = []
                    heap      = M.empty
                    pc        = 0
                    jumptable = M.empty
                 in VM stack heap instrs pc jumptable


runWhiteSpace :: FilePath -> IO ()
runWhiteSpace file = do
    content <- readFile file
    case parse (many impParser) "Whitespace" content of
      Left  err -> print err
      Right val -> do mapM_ (putStrLn.show) val
                      eval $ initVM val
                      putStr "\n"


eval :: VM -> IO VM
eval vm = let index   = pc vm
              codeMem = instructions vm
              size    = length codeMem
           in if index >= size
                 then return vm
                 else let instr = codeMem !! index
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

