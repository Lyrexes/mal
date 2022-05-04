import System.IO (hFlush, stdout)
import Readline (addHistory, readline, loadHistory)
import Data.Foldable(mapM_)
import Data.Maybe(isNothing, fromJust)
import Control.Monad ( unless, when, void, join )
import Types(Result, MalType)
import Reader (readStr)
import Printer (prStr)

malInput :: String -> IO(Maybe String)
malInput prompt = do readline prompt

malRead :: String -> Result MalType
malRead = readStr

malEval :: Result MalType -> Result MalType
malEval ast = ast

malPrint :: Result MalType -> String
malPrint (Right malVal) = prStr malVal True
malPrint (Left err) = err

malREP :: String -> String
malREP = malPrint . malEval . malRead

replLoop :: String -> IO()
replLoop prompt = do
    user_input <- malInput prompt
    unless (isNothing user_input) $ do
        case malREP $fromJust user_input of 
            ""   -> replLoop prompt
            str  -> putStrLn str
        replLoop prompt

main :: IO()
main = replLoop "user> "
