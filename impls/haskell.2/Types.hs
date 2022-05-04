module Types (MalType(..), MalError, Result, keyValListToMap, toKeyValList, decodeKey,malComp, malFst, malLst) where

import Data.Map.Strict (Map, fromList, toList)

type MalError = String
type Result a = Either MalError a

data MalType =  MalBool Bool  |
                MalString String |
                MalKeyword String |
                MalSymbol String |
                MalVector [MalType] |
                MalList [MalType] |
                MalMap (Map String MalType) |
                MalNumber Integer |
                MalFn ([MalType] -> Result MalType) |
                MalNil

malComp :: MalType -> MalType -> Bool
malComp (MalBool a) (MalBool b)       = a == b
malComp (MalString a) (MalString b)   = a == b
malComp (MalVector a) (MalVector b)   = seqComp a b
malComp (MalList a) (MalVector b)   = seqComp a b
malComp (MalVector a) (MalList b)   = seqComp a b
malComp (MalList a) (MalList b)       = seqComp a b
malComp (MalMap a) (MalMap b)         = mapComp (toList a) (toList b)
malComp (MalNumber a) (MalNumber  b)  = a == b
malComp (MalSymbol a) (MalSymbol b)   = a==b
malComp (MalKeyword a) (MalKeyword b) = a == b
malComp MalNil MalNil                 = True
malComp _ _                           = False 

seqComp :: [MalType] -> [MalType] -> Bool
seqComp (x:xs) (y:ys) = malComp x y && seqComp xs ys
seqComp [] [] = True
seqComp _  [] = False
seqComp [] _  = False

mapComp :: [(String, MalType)] -> [(String, MalType)] -> Bool
mapComp ((sx,mx):xs) ((sy,my):ys) = sy == sx && malComp mx my && mapComp xs ys
mapComp [] [] = True
mapComp _  [] = False
mapComp [] _  = False

malFst :: MalType -> MalType
malFst (MalList (x:xs)) = x
malFst (MalVector (x:xs)) = x
malFst _ = error "malFst: not a sequence!"

malLst :: MalType -> MalType
malLst (MalList (x:xs)) = last (x:xs)
malLst (MalVector (x:xs)) = last (x:xs)
malLst _ = error "malFst: not a sequence!"

decodeKey :: String -> MalType
decodeKey (x:xs)| x == 'k' = MalKeyword xs
                | x == 's' = MalString xs
                | otherwise = error ("cant decode key: " ++ (x:xs))
decodeKey [] = error "cant decode key because its empty!"

keyValListToMap :: Maybe [(String, MalType)] -> Maybe MalType
keyValListToMap = fmap (MalMap . fromList) 

concatMaybe :: Maybe [a] -> Maybe [a] -> Maybe [a]
concatMaybe (Just x) (Just y) = Just (x ++ y)
concatMaybe Nothing (Just x) =  Just x
concatMaybe (Just x) Nothing =  Just x
concatMaybe _ _ = Nothing

toKeyValList :: [MalType] -> Maybe [(String, MalType)]
toKeyValList ((MalString str):y:xs) = concatMaybe (Just [('s':str, y)]) (toKeyValList xs)
toKeyValList ((MalKeyword kw):y:xs) = concatMaybe (Just [('k':kw, y)]) (toKeyValList xs)
toKeyValList [] = Just []
toKeyValList _ = Nothing