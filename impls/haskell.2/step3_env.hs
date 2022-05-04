{-# LANGUAGE TupleSections #-}
import System.IO (hFlush, stdout)
import Readline (addHistory, readline, loadHistory)
import Data.Foldable(mapM_)
import Data.Maybe(isNothing, fromJust)
import Data.Map(Map, fromList, lookup, toList)
import Control.Monad ( unless, when, void, join )
import Reader (readStr)
import Printer (prStr)
import Types
    ( Result,
      MalType(MalNumber, MalFn, MalList, MalVector, MalSymbol,
              MalMap, MalString) )
import Env ( Env (Env), envGet, envSet, emptyEnv )
import qualified Data.Bifunctor as Bifunc
type MalFn = [MalType] -> Result MalType
type IntFn = Integer -> Integer -> Integer

malNumOp :: String -> IntFn -> MalFn
malNumOp _ op [MalNumber x, MalNumber y] = Right (MalNumber (op x y))
malNumOp sym _ _= Left ("invalid arguments for operation: " ++ sym)

replEnv :: Env
replEnv = Env Nothing (fromList [
        ("+", MalFn(malNumOp "+" (+))),
        ("-", MalFn(malNumOp "-" (-))),
        ("*", MalFn(malNumOp "*" (*))),
        ("/", MalFn(malNumOp "/" div))
    ])

malInput :: String -> IO(Maybe String)
malInput prompt = do readline prompt

malRead :: String -> Result MalType
malRead = readStr

malEval :: MalType -> Env -> Result (MalType,Env)
malEval (MalList lst) env
    | null lst = Right (MalList lst, env)
    | otherwise = malApply lst env
malEval ast env = evalAst ast env >>= (\x -> Right(x, env))

malPrint :: MalType -> String
malPrint malVal = prStr malVal True

malREP :: String -> Env -> Result (String,Env)
malREP str env = Bifunc.first malPrint <$> (malRead str >>= (`malEval` env))

replLoop :: String -> Env -> IO()
replLoop prompt env = do
    user_input <- malInput prompt
    unless (isNothing user_input) $ do
        case malREP (fromJust user_input) env of
          Left s ->case s of
              "" -> replLoop prompt env
              st -> do
                  putStrLn st
                  replLoop prompt env
          Right (s, new_env) -> case s of
              "" -> replLoop prompt new_env
              st -> do
                  putStrLn st
                  replLoop prompt new_env

main :: IO()
main = replLoop "user> " replEnv

malApply :: [MalType] -> Env -> Result (MalType,Env)
malApply ((MalSymbol "def!"):xs) e = applyDef xs e
malApply ((MalSymbol "let*"):xs) e = applyLet xs e
malApply ast env = case evalAst (MalList ast) env of
    Right (MalList ((MalFn f):xs)) ->  (,env) <$> f xs
    Right val -> Left("expected callable and a list got: "++prStr val True)
    Left err -> Left err

applyDef :: [MalType] -> Env -> Result (MalType,Env)
applyDef [MalSymbol k, v] env = (\(x,e) -> (x, envSet (k,x) e)) <$> malEval v env
applyDef _ _ = Left "invalid def! statement, expected symbol and value!"

applyLet :: [MalType] -> Env -> Result (MalType,Env)
applyLet [MalList b, expr] e = (\(x,_)->(x, e)) <$> (letEnv b (emptyEnv e) >>= malEval expr)
applyLet [MalVector b, expr] e = (\(x,_)->(x, e)) <$> (letEnv b (emptyEnv e) >>= malEval expr)
applyLet _ _ = Left "invalid let* statement, expected bind list and expression"

letEnv :: [MalType] -> Env -> Result Env
letEnv ((MalSymbol k):v:xs) env = malEval v env >>= (\(x,e) -> letEnv xs (envSet (k, x) e))
letEnv [] env = Right env
letEnv _ _  = Left "invalid binding list in let* statement"

evalAst :: MalType -> Env -> Result MalType
evalAst (MalList l) env   = evalSeq MalList l env
evalAst (MalVector v) env = evalSeq MalVector v env
evalAst (MalMap m) env    = evalMap m env
evalAst (MalSymbol s) env = envLookup s env
evalAst ast _ = Right ast

envLookup :: String -> Env -> Result MalType
envLookup sym env = case envGet sym env of
    Just val -> Right val
    Nothing -> Left (sym++" not found")

evalSeq :: ([MalType] -> MalType) -> [MalType] -> Env -> Result MalType
evalSeq f l env = fmap f (eval l env)
    where
        eval:: [MalType] -> Env -> Result  [MalType]
        eval (x:xs) e = eval xs e >>= (\lst -> (\(v,_) -> v:lst) <$> malEval x e)
        eval [] _ = Right []

evalMap :: Map String MalType -> Env -> Result MalType
evalMap m env  = fmap (MalMap . fromList) (eval (toList m) env)
    where
        eval:: [(String,MalType)] -> Env -> Result  [(String,MalType)]
        eval ((s,x):xs) e = eval xs e >>= (\l -> (\(v,_) -> (s,v):l) <$> malEval x e) 
        eval [] _ = Right []
