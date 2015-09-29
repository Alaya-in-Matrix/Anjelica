module Language.Whitespace where

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative hiding((<|>))
import System.Environment

data IMP = StackManip | Arithmetic | HeapAccess | FlowCtrl | IO deriving(Show, Eq)

wsSpace = char ' '
wsTab   = char '\t'
wsLf    = char '\n'

impParser :: Parser IMP
impParser = (FlowCtrl <$ wsLf) 
        <|> (StackManip <$ wsSpace) 
        <|> (wsTab *> (Arithmetic <$ wsSpace 
                    <|> HeapAccess <$ wsTab 
                    <|> IO <$ wsLf))

