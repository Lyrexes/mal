module Env (Env(..), envSet, envGet, emptyEnv, withBinds)where

import Data.Map(Map, fromList, lookup, toList, empty,insert, member)
import Types ( MalType (MalSymbol), Result )
import Printer (prStr)
import Data.Maybe ( fromJust )
import Data.IORef(IORef, modifyIORef, readIORef)
import Control.Monad.ST ( ST, runST )
import Control.Monad ( join )

data Env = Env (Maybe Env) (Map String MalType)

emptyEnv :: Env -> Env
emptyEnv e = Env (Just e) empty

withBinds :: Env -> [String] -> [MalType] -> Env
withBinds e b v = Env (Just e) (fromList (zip b v))

envSet :: (String, MalType) -> Env -> Env
envSet (k, v) (Env a m) = Env a (insert k v m)

envFind  :: String -> Env  -> Maybe Env
envFind k (Env (Just e) m) = if member k m then
                                 Just (Env (Just e) m)
                            else envFind k e
envFind k (Env Nothing m)  = if member k m then
                                Just (Env Nothing m)
                            else Nothing

envGet :: String -> Env -> Maybe MalType
envGet k e =  envFind k e >>= (\(Env _ m) -> Data.Map.lookup k m) 