module Reader (readStr) where

import Text.ParserCombinators.Parsec (Parser, parse, string, char, 
    digit ,anyChar, (<|>), oneOf, sepBy,  noneOf, count, many, many1, manyTill)
import Data.Map.Strict (fromList)
import Types

posNumberParser :: Parser MalType
posNumberParser = MalNumber . read <$> many1 digit

negNumberParser :: Parser MalType
negNumberParser = MalNumber . negate . read <$> many1 digit

symbolParser :: Parser MalType
symbolParser = readSym <$> many1 allowedChar

keywordParser :: Parser MalType
keywordParser = MalKeyword <$> (char ':' *> many1 allowedChar)

readSym :: String -> MalType
readSym "true" = MalBool True
readSym "false" = MalBool False
readSym "nil" = MalNil
readSym s  = MalSymbol s

stringParser :: Parser MalType
stringParser = MalString <$> (char '"' *> many stringChar <* char '"')

stringChar :: Parser Char
stringChar = (unescapeChar <$> (char '\\' *> anyChar))<|> noneOf "\""

unescapeChar :: Char -> Char
unescapeChar 'n' =  '\n'
unescapeChar c = c

sep :: Parser [Char]
sep = many (oneOf " \n,")

listParser :: Parser MalType
listParser = MalList <$> (char '(' *> sep *> sepBy formParser sep  <* char ')')

vecParser :: Parser MalType
vecParser = MalVector <$> (char '[' *> sep *> sepBy formParser sep <* char ']')

mapParser :: Parser MalType
mapParser = readMap =<< char '{' *> sep *> sepBy formParser sep <* char '}'

readMap :: [MalType] -> Parser MalType
readMap list = case (keyValListToMap . toKeyValList) list of
        Just x -> return x
        Nothing -> fail "invalid map!"

unquoteParser :: Parser MalType
unquoteParser = char '~' *> (
                    withList lForm <$> (char '@' *>  formParser) <|>
                    withList sForm <$> formParser 
                )
                where   lForm = "splice-unquote"
                        sForm = "unquote"

specialFormParser :: Parser MalType
specialFormParser = specialForm "@" "deref"           <|>
                    specialForm "`" "quasiquote"      <|>
                    specialForm "'" "quote"           <|>
                    unquoteParser                     <|>
                    withMetaParser

withMetaParser :: Parser MalType
withMetaParser =  withMeta <$> (char '^' *> count 2 formParser)
    where withMeta :: [MalType] -> MalType
          withMeta x =  MalList [MalSymbol "with-meta", last x, head x]

specialForm:: String -> String -> Parser MalType 
specialForm s l = withList l <$> (string s *> formParser)

withList :: String -> MalType -> MalType
withList l x = MalList [MalSymbol l, x]
        
allowedChar :: Parser Char
allowedChar = noneOf "\n\r \"(),;[\\]{}"

minusParser :: Parser MalType
minusParser = char '-' *> (negNumberParser <|> 
              MalSymbol . ('-':) <$> many allowedChar)

atomParser :: Parser MalType
atomParser =    posNumberParser   <|>
                minusParser       <|>
                keywordParser     <|>
                stringParser      <|>
                specialFormParser <|>
                symbolParser   

commentParser :: Parser MalType
commentParser = char ';' *> manyTill anyChar (char '\n') *> formParser

formParser :: Parser MalType
formParser = sep *>(
                commentParser <|>
                listParser    <|>
                mapParser     <|>
                vecParser     <|>
                atomParser
            )<* sep

readStr :: String -> Result MalType
readStr str |  null str = Left ""
readStr str = case parse formParser "Mal" str of
            Left err -> Left (show err)
            Right val -> Right val
