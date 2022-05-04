import System.IO (hFlush, stdout)
import Readline (addHistory, readline, loadHistory)
import Data.Foldable(mapM_)
import Data.Maybe(isNothing, fromJust)
import Data.Map(Map, fromList, lookup, toList)
import Control.Monad ( unless, when, void, join )
import Reader (readStr)
import Printer (prStr)
import Types
    ( MalType(MalNumber, MalFn, MalList, MalVector, MalSymbol, MalMap),
      Result )
import qualified Data.Map as Map

type MalFn = [MalType] -> Result MalType
type IntFn = Integer -> Integer -> Integer
type Env   = Map String MalType

malNumOp :: String -> IntFn -> MalFn
malNumOp _ op [MalNumber x, MalNumber y] = Right (MalNumber (op x y))
malNumOp sym _ _= Left ("invalid arguments for operation: " ++ sym)

replEnv :: Env
replEnv = fromList [
        ("+", MalFn(malNumOp "+" (+))),
        ("-", MalFn(malNumOp "-" (-))),
        ("*", MalFn(malNumOp "*" (*))),
        ("/", MalFn(malNumOp "/" div))
    ]

malInput :: String -> IO(Maybe String)
malInput prompt = do readline prompt

malRead :: String -> Result MalType
malRead = readStr

malEval :: Result MalType -> Env -> Result MalType
malEval (Right ast@(MalList lst)) env
            | null lst  = Right ast
            | otherwise = malApply (evalAst ast env)
malEval (Right ast) env = evalAst ast env
malEval (Left err)   _  = Left err

malPrint :: Result MalType -> String
malPrint (Right malVal) = prStr malVal True
malPrint (Left err) = err

malREP :: String -> Env -> String
malREP str env = malPrint (malEval (malRead str) env)

replLoop :: String -> IO()
replLoop prompt = do
    user_input <- malInput prompt
    unless (isNothing user_input) $ do
        case malREP (fromJust user_input) replEnv of
            ""   -> replLoop prompt
            str  -> putStrLn str
        replLoop prompt

main :: IO()
main = replLoop "user> "

malApply :: Result MalType -> Result MalType
malApply (Right (MalList ((MalFn f):xs))) = f xs
malApply (Right a) = error ("malApply expected list! got: " ++ prStr a True)
malApply err@(Left _) = err

evalAst :: MalType -> Env -> Result MalType
evalAst (MalList l) env   = evalSeq MalList l env
evalAst (MalVector v) env = evalSeq MalVector v env
evalAst (MalMap m) env    = evalMap m env
evalAst (MalSymbol s) env = envLookup s env
evalAst ast _ = Right ast

envLookup :: String -> Env -> Result MalType
envLookup sym env = case Map.lookup sym env of
    Just val -> Right val
    Nothing -> Left ("symbol not found: " ++ sym)

evalSeq :: ([MalType] -> MalType) -> [MalType] -> Env -> Result MalType
evalSeq f l env = fmap f (eval l env)
    where
        eval:: [MalType] -> Env -> Result  [MalType]
        eval (x:xs) e = eitherAppend (eval xs e) (malEval (Right x) e)
        eval [] _ = Right []

evalMap :: Map String MalType -> Env -> Result MalType
evalMap m env  = fmap (MalMap . fromList) (eval (toList m) env)
    where
        eval:: [(String,MalType)] -> Env -> Result  [(String,MalType)]
        eval ((s,x):xs) e = eitherMapAppend (eval xs e) (s,malEval (Right x) e)
        eval [] _ = Right []

eitherMapAppend :: Either a [(b,c)] -> (b, Either a c)  -> Either a [(b,c)]
eitherMapAppend (Right x) (b, Right c) = Right ((b, c):x)
eitherMapAppend err@(Left _) _ =  err
eitherMapAppend _ (_, Left err) = Left err

eitherAppend :: Either a [b] -> Either a b -> Either a [b]
eitherAppend (Right x) (Right a) = Right (a:x)
eitherAppend err@(Left _) _ = err
eitherAppend _ (Left err) = Left err

