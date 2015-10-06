module Language.WhiteSpace(
    interpString
    , interpFile
  )where

-- Author: lvwenlong_lambda@qq.com
-- Last Modified:2015年10月06日 星期二 23时21分01秒 二

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative hiding((<|>), many)
import System.Environment
import Data.Maybe
import Numeric (showIntAtBase)
import Data.Char(intToDigit)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M hiding ((!))

(!)  = (V.!)
(!?) = (V.!?)

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
data CtrlCmd = Label    Integer
             | Call     Integer
             | Jump     Integer
             | JumpZero Integer
             | JumpNeg  Integer
             | FuncEnd
             | ProgEnd  deriving(Eq, Show)

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
            <|> ProgEnd <$ (wsLf *> wsLf)

data VM = VM {
    stack :: [Integer] , 
    heap :: M.Map Integer Integer,
    instructions :: V.Vector IMP , -- This should be an array(vector) instead of a list
    pc :: Int , 
    jumptable :: M.Map Integer Int,
    retStack :: [Int]
} deriving(Eq, Show)

initVM :: [IMP] -> VM
initVM instrs = let stack     = []
                    heap      = M.empty
                    pc        = 0
                    jumptable = genJumpTable instrs
                    retStack  = []
                 in VM stack heap (V.fromList instrs) pc jumptable retStack


genJumpTable :: [IMP] -> M.Map Integer Int
genJumpTable imps = foldr mInsert M.empty (zip [(0::Int)..] imps)
  where mInsert :: (Int,IMP) -> M.Map Integer Int -> M.Map Integer Int
        mInsert (pc, imp) map = case imp of
                                  Control (Label n) -> M.insert n pc map
                                  _  -> map

newStack f (VM s h i p j r) = VM (f s) h i p j r
newHeap  f (VM s h i p j r) = VM s (f h) i p j r
newInstr f (VM s h i p j r) = VM s h (f i) p j r
newPC    f (VM s h i p j r) = VM s h i (f p) j r
newJumpT f (VM s h i p j r) = VM s h i p (f j) r
newRetS  f (VM s h i p j r) = VM s h i p j (f r)

incPC :: VM -> IO VM
incPC vm = return $ newPC (+1) vm

eval :: VM -> IO VM
eval vm = let index = pc vm
              codeMem = instructions vm
              instr = codeMem !? index
           in case instr of 
                Nothing                 -> return vm
                Just (Control ProgEnd)  -> return vm
                Just ins                -> evalInstr vm ins >>= eval

evalInstr :: VM -> IMP -> IO VM
evalInstr vm (Stack (Push num)) = incPC $ newStack (num:) vm
evalInstr vm (Stack Duplicate)  = incPC $ newStack ((head $ stack vm):) vm
evalInstr vm (Stack Pop)        = incPC $ newStack tail vm
evalInstr vm (Stack Swap)       = incPC $ newStack swapTop vm
  where swapTop []  = error "empty list"
        swapTop [a] = error "single element list"
        swapTop (a:b:xs) = (b:a:xs)
evalInstr vm (Arithmetic arithOp) = incPC $ newStack op vm
  where op []         = error "empty list"
        op [x]        = error "single element list"
        op (a:b:rest) = (a `fuck` b) : rest
        fuck          = case arithOp of Add -> (+)
                                        Sub -> (-)
                                        Mul -> (*)
                                        Div -> div
                                        Mod -> mod
evalInstr vm (Heap Store) = incPC $ newHeap (M.insert addr val) vm
  where (addr:val:_) = stack vm
evalInstr vm (Heap Retrieve) = incPC $ newStack retrieve vm
  where retrieve (addr:rest) = let val = M.lookup addr (heap vm)
                                in case val of
                                     Nothing  -> error $ "Can't retrieve addr " ++ (show addr) ++ " from heap"
                                     Just val -> val : rest
evalInstr vm (IO OutChar)  = do putChar $ toEnum $ fromEnum $ head $ stack vm
                                incPC vm
evalInstr vm (IO OutInt)   = do putStr $ show $ head $ stack vm
                                incPC vm
evalInstr vm (IO ReadChar) = do int <- (toInteger . fromEnum) <$> getChar
                                incPC $ newStack (int:) vm
evalInstr vm (IO ReadInt)  = do int <- read <$> getLine
                                incPC $ newStack (int:) vm
evalInstr vm (Control (Label label)) = incPC vm -- Labels should already have been inserted into jumpTable
evalInstr vm (Control (Call label))  = return $ (pcf . retSf) vm
  where pcf     = newPC (const $ fromJust $ M.lookup label (jumptable vm))
        retSf   = newRetS (1+(pc vm):)
        labelPC = case M.lookup label (jumptable vm) of
                    Nothing -> error $ "Can't find corresponding PC in jumptable for label " ++ show label
                    Just p  -> p
evalInstr vm (Control (Jump label))  = return $ newPC pcf vm
  where pcf = let newPC = M.lookup label (jumptable vm)
               in case newPC of
                    Nothing -> error $ "can not find label for Jump " ++ show label
                    Just v  -> const v
evalInstr vm (Control (JumpNeg label)) = return $ newPC pcf vm
  where pcf = let top = head $ stack vm
               in if top >= 0 
                     then (+1)
                     else let newPC = M.lookup label (jumptable vm)
                           in case newPC of
                                Nothing -> error $ "can not find label for JumpNeg " ++ show label 
                                Just v  -> const v
evalInstr vm (Control (JumpZero label)) = return $ newPC pcf vm
  where pcf = let top = head $ stack vm
               in if top /= 0 
                     then (+1)
                     else let newPC = M.lookup label (jumptable vm)
                           in case newPC of
                                Nothing -> error $ "can not find label for JumpNeg " ++ show label 
                                Just v  -> const v
evalInstr vm (Control FuncEnd) = return $ (pcf . retSf) vm
  where retSf = newRetS tail
        pcf   = newPC (const $ head $ retStack vm) 
evalInstr vm (Control ProgEnd) = undefined

interpString :: String -> IO ()
interpString wsCode = do let content = filter (`elem` " \t\n") wsCode
                         case parse (many impParser) "Whitespace" content of
                           Left  err   -> print err
                           Right exprs -> do mapM_ (putStrLn.show) exprs
                                             putStrLn "==============================="
                                             () <$ (eval $ initVM exprs)
interpFile :: FilePath -> IO ()
interpFile file = readFile file >>= interpString


fibimp :: [IMP]
fibimp = [
    IO ReadInt
    , Stack Duplicate
    , Control (Call fib)
    , IO OutInt
    , Stack (Push 10) 
    , IO OutChar
    , Control ProgEnd

    , Control (Label fib) -- function to calculate fib
        , Stack (Push (-2))   -- test n < 2
        , Arithmetic Add
        , Control (JumpNeg fibRecursionOut)
        , Stack Duplicate
        , Control (Call fib)  -- fib (n-2)
        , Stack Swap
        , Stack (Push 1)
        , Arithmetic Add     -- n-2+1
        , Control (Call fib)  -- fib(n-1)
        , Arithmetic Add
        , Control (Jump fibEnd)
    , Control (Label fibRecursionOut)
        , Stack Pop
        , Stack (Push 1)
    , Control (Label fibEnd)
    , Control FuncEnd
    ] where fib             = 0
            fibRecursionOut = 1
            fibEnd          = 2

fac :: [IMP]
fac = [
    IO ReadInt
    , Stack Duplicate
    , Control (Call facLabel)
    , IO OutInt
    , Stack (Push 10)
    , IO OutChar
    , Control ProgEnd

    , Control (Label facLabel)          -- function to calculate fac number
        , Control (JumpZero facOutRecursionLabel)
        , Stack Duplicate
        , Control (Call subOneLabel)
        , Control (Call facLabel)           -- call fac(n-1)
        , Arithmetic Mul
        , Control (Jump facEndLabel)
    , Control (Label facOutRecursionLabel)
        , Stack Pop
        , Stack (Push 1)
    , Control (Label facEndLabel)
    , Control FuncEnd

    , Control (Label subOneLabel)     -- function to subtract one from stack top
        , Stack (Push (-1))
        , Arithmetic Add
    , Control FuncEnd
    ] where facLabel             = 0
            facOutRecursionLabel = 1
            facEndLabel          = 2
            subOneLabel          = 3

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
    disAssembly (Arithmetic para) = "\t "  ++ disAssembly para
    disAssembly (Heap para)       = "\t\t" ++ disAssembly para
    disAssembly (IO para)         = "\t\n" ++ disAssembly para
    disAssembly (Control para)    = "\n"   ++ disAssembly para
