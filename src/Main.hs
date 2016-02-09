module Main where
import qualified Language.BrainFuck            as B
import qualified Language.ElementaryArithmetic as E
import qualified Language.WhiteSpace           as W
import           System.Environment

main :: IO ()
main = do
    (lang:filePath:_) <- getArgs
    let interpFile | lang == "-ws" || lang == "--whitespace" = W.interpFile
                   | lang == "-bf" || lang == "--brainfuck"  = B.interpFile
                   | lang == "-ea" || lang == "--arithmetic" = E.interpFile
                   | otherwise = error $ "Invalid language"
      in interpFile filePath


