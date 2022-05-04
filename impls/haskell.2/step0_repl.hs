import System.IO (hFlush, stdout)
import Readline (addHistory, readline, loadHistory)
import Data.Foldable(mapM_)
import Data.Maybe(isNothing)
import Control.Monad (unless, when)

type MalType = String

malInput :: String -> IO(Maybe String)
malInput prompt = do readline prompt

malRead :: String -> MalType
malRead ast = ast

malEval :: MalType -> MalType
malEval ast = ast

malPrint :: MalType -> String
malPrint ast = ast

malREP :: String -> String
malREP = malPrint . malEval . malRead

replLoop :: String -> IO()
replLoop prompt = do
    user_input <- malInput prompt
    unless (isNothing user_input) $ do
        mapM_ (putStrLn . malREP) user_input
        replLoop prompt

main :: IO ()
main = do
    replLoop "user> "