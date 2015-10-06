module Main where
import System.Environment
import qualified Language.WhiteSpace as W
import qualified Language.BrainFuck as B
 
main :: IO ()
main = do   
    (lang:filePath:_) <- getArgs
    let interpFile | lang == "-ws" || lang == "--whitespace" = W.interpFile
                   | lang == "-bf" || lang == "--brainfuck"  = B.interpFile
                   | otherwise = error $ "Invalid language"
      in interpFile filePath
    


