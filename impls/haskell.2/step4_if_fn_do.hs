{-# LANGUAGE TupleSections #-}
import System.IO (hFlush, stdout)
import Readline (addHistory, readline, loadHistory)
import Data.Foldable(mapM_)
import Data.Maybe(isNothing, fromJust)
import Data.Map(Map, fromList, lookup, toList)
import Control.Monad ( unless, when, void, join )
import Reader (readStr)
import Printer (prStr)
import Core (ns)
import Types
    ( Result,
      MalType(MalNumber, MalFn, MalList, MalVector, MalSymbol,
              MalMap, MalString, MalNil, MalBool), malFst, malLst )
import Env ( Env (Env), envGet, envSet, emptyEnv, withBinds )
import qualified Data.Bifunctor as Bifunc

malInput :: String -> IO(Maybe String)
malInput prompt = do readline prompt

malRead :: String -> Result MalType
malRead = readStr

malEval :: MalType -> Env -> Result (MalType,Env)
malEval (MalList lst) env
    | null lst = Right (MalList lst, env)
    | otherwise = malApply lst env
malEval ast env = evalAst ast env

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
main = replLoop "user> " ns

malApply :: [MalType] -> Env -> Result (MalType,Env)
malApply (MalSymbol "def!":xs) e = applyDef xs e
malApply ((MalSymbol "let*"):xs) e = applyLet xs e
malApply ((MalSymbol "do"):xs)   e = applyDo  xs e
malApply ((MalSymbol "if"):cond:xs) e = applyIf cond xs e
malApply ((MalSymbol "fn*"):xs) e = createFn xs e
malApply ast env = case evalAst (MalList ast) env of
    Right (MalList ((MalFn f):xs),ne) ->  (,ne) <$> f xs
    Right (val,_) -> Left("expected valid expression got: "++prStr val True)
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

applyDo :: [MalType] -> Env -> Result (MalType,Env)
applyDo x e | null x = Right (MalNil, e)
            | otherwise = Bifunc.first malLst <$> evalAst (MalList x) e

applyIf :: MalType -> [MalType] -> Env -> Result (MalType,Env)
applyIf  cond brnch e =  malEval cond e >>= (\(x,ne) -> evalIf x brnch ne)
    where
        evalIf :: MalType -> [MalType] -> Env -> Result (MalType,Env)
        evalIf MalNil [_,els] env = malEval els env
        evalIf MalNil [_] env = Right (MalNil,env)
        evalIf (MalBool False) [_,els] env = malEval els env
        evalIf (MalBool False) [_] env = Right (MalNil,env)
        evalIf _ (b:_) env = malEval b env
        evalIf c b _ = Left ("invalid if expression got: "++prStr (MalList(c:b)) True)

createFn :: [MalType] -> Env -> Result (MalType,Env)
createFn [MalList binds, body] e = let newMalFn b = MalFn(\expr -> fst <$> malEval body (withBinds e b expr))
     in (\b -> (newMalFn b, e)) <$> fnBinds binds
createFn [MalVector binds, body] e = let newMalFn b = MalFn(\expr -> fst <$> malEval body (withBinds e b expr))
     in (\b -> (newMalFn b, e)) <$> fnBinds binds
createFn xs _ = Left ("expected: fn* (params) body, got: " ++ prStr (MalList xs) True)

fnBinds :: [MalType] -> Result [String]
fnBinds ((MalSymbol s):xs) =  (s :) <$> fnBinds xs
fnBinds [] = Right []
fnBinds s = Left ("invalid binds in fn expression, got: "++prStr (MalList s) True)

evalAst :: MalType -> Env -> Result (MalType,Env)
evalAst (MalList l) env   = Bifunc.first (MalList . reverse) <$> evalSeq (reverse l) env
evalAst (MalVector v) env = Bifunc.first (MalVector . reverse) <$> evalSeq (reverse v) env
evalAst (MalMap m) env    = evalMap m env
evalAst (MalSymbol s) env = (,env) <$> envLookup s env
evalAst ast e = Right (ast,e)

envLookup :: String -> Env -> Result MalType
envLookup sym env = case envGet sym env of
    Just val -> Right val
    Nothing -> Left (sym++" not found")

evalSeq :: [MalType] -> Env -> Result ([MalType],Env)
evalSeq (x:xs) e = evalSeq xs e >>= (\(lst,oe) -> (\(v,ne) -> (v:lst,ne)) <$> malEval x oe)
evalSeq [] e = Right ([] ,e)

evalMap :: Map String MalType -> Env -> Result (MalType, Env)
evalMap m env  = Bifunc.first (MalMap . fromList . reverse) <$> eval ((reverse . toList) m) env
    where
        eval:: [(String,MalType)] -> Env -> Result  ([(String,MalType)], Env)
        eval ((s,x):xs) e = eval xs e >>= (\(l,oe) -> (\(v,ne) -> ((s,v):l,ne)) <$> malEval x oe)
        eval [] e = Right ([],e)
