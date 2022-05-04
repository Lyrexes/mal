module Core (ns) where

import Types ( MalType(MalFn, MalNumber, MalNil, MalList), MalType(..), Result, malComp )
import Printer (prStr)
import Env (Env, Env (..))
import Data.Map (fromList)

type MalFn = [MalType] -> Result MalType
type IntFn = Integer -> Integer -> Integer

malNumOp :: String -> IntFn -> MalFn
malNumOp _ op [MalNumber x, MalNumber y] = Right (MalNumber (op x y))
malNumOp sym _ _= Left ("invalid arguments for operation: " ++ sym)

ns :: Env
ns = Env Nothing (fromList [
        ("+", MalFn(malNumOp "+" (+))),
        ("-", MalFn(malNumOp "-" (-))),
        ("*", MalFn(malNumOp "*" (*))),
        ("/", MalFn(malNumOp "/" div)),
        ("list", MalFn(list)),
        ("list?", MalFn(isList)),
        ("empty?", MalFn(isEmpty)),
        ("count", MalFn(count)),
        ("=", MalFn(eq)),
        (">", MalFn(gt)),
        ("<", MalFn(lt)),
        (">=", MalFn(gtEq)),
        ("<=", MalFn(ltEq))
    ])

list :: [MalType] -> Result MalType
list xs = Right (MalList xs)

isList :: [MalType] -> Result MalType
isList ((MalList _):_) = Right (MalBool True)
isList _ = Right (MalBool False)

isEmpty :: [MalType] -> Result MalType
isEmpty ((MalList []):_) = Right (MalBool True)
isEmpty ((MalList xs):_) = Right (MalBool False)
isEmpty (x:_) = Left ("isEmpty expected a list got " ++ (prStr x True))

count :: [MalType] -> Result MalType
count ((MalList xs):_) = Right (MalNumber (toInteger (length xs)))
count (x:_) = Left ("isEmpty expected a list got " ++ (prStr x True))

eq :: [MalType] -> Result MalType
eq (x:y:_) = Right (MalBool (malComp x y))
eq _ = Left ("'=' : expected more parameters!")

gt :: [MalType] -> Result MalType
gt ((MalNumber x):(MalNumber y):_) = Right (MalBool (x > y))
gt _ = Left ("'>' : expected 2 numbers got!")

lt :: [MalType] -> Result MalType
lt ((MalNumber x):(MalNumber y):_) = Right (MalBool (x < y))
lt _ = Left ("'<' : expected 2 numbers got!")

gtEq :: [MalType] -> Result MalType
gtEq ((MalNumber x):(MalNumber y):_) = Right (MalBool (x >= y))
gtEq _ = Left ("'>=' : expected 2 numbers got!")

ltEq :: [MalType] -> Result MalType
ltEq ((MalNumber x):(MalNumber y):_) = Right (MalBool (x <= y))
ltEq _ = Left ("'<=' : expected 2 numbers got!")