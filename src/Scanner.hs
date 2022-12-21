module Scanner where
import Text.ParserCombinators.Parsec

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

data LoxToken
-- Single-character tokens.
 = LEFT_PAREN
 | RIGHT_PAREN
 | LEFT_BRACE
 | RIGHT_BRACE
 | COMMA
 | DOT
 | MINUS
 | PLUS
 | SEMICOLON
 | SLASH
 | STAR
-- 1-2 character tokens.
 | BANG
 | BANG_EQUAL
 | EQUAL 
 | EQUAL_EQUAL
 | GREATER
 | GREATER_EQUAL
 | LESS
 | LESS_EQUAL
-- Literals.
 | IDENTIFIER String 
 | STRING String 
 | NUMBER String
-- Keywords.
 | AND 
 | CLASS 
 | ELSE 
 | FALSE
 | FUN
 | FOR
 | IF 
 | NIL
 | OR
 | PRINT 
 | RETURN 
 | SUPER
 | THIS
 | TRUE 
 | VAR
 | WHILE 
 | EOF
  deriving (Show, Eq)


basicToken :: Parsec String () LoxToken
basicToken = 
  (char '(' >> return LEFT_PAREN) <|>
  (char ')' >> return RIGHT_PAREN) <|>
  (char '{' >> return LEFT_BRACE) <|>
  (char '}' >> return RIGHT_BRACE) <|>
  (char ',' >> return COMMA) <|>
  (char '.' >> return DOT) <|>
  (char '-' >> return MINUS) <|>
  (char '+' >> return PLUS) <|>
  (char ';' >> return SEMICOLON) <|>
  (char '*' >> return STAR) <|>
  (char '!' >> return BANG) <|>
  (char '=' >> return EQUAL) <|>
  (char '>' >> return GREATER) <|> 
  (char '<' >> return LESS)

ambiguousToken :: Parsec String () LoxToken
ambiguousToken = 
  (string "!=" >> return BANG_EQUAL) <|>
  (string "<=" >> return LESS_EQUAL) <|> 
  (string ">=" >> return GREATER_EQUAL) <|>
  (string "==" >> return EQUAL_EQUAL)


keywordToken :: Parsec String () LoxToken
keywordToken = 
  (string "and" >>    return AND) <|>
  (string "class" >>  return CLASS) <|>
  (string "else" >>   return ELSE) <|>
  (string "false" >>  return FALSE) <|>
  (string "for" >>    return FOR) <|>
  (string "fun" >>    return FUN) <|>
  (string "if" >>     return IF) <|>
  (string "nil" >>    return NIL) <|>
  (string "or" >>     return OR) <|>
  (string "print" >>  return PRINT) <|>
  (string "return" >> return RETURN) <|>
  (string "super" >>  return SUPER) <|>
  (string "this" >>   return THIS) <|>
  (string "true" >>   return TRUE) <|>
  (string "var"   >>  return VAR) <|>
  (string "while" >>  return WHILE)

literalToken :: Parsec String () LoxToken
literalToken = stringToken <|> numberToken <|> identifierToken where
  stringToken = STRING <$> (between (char '"') (char '"') (many (escapedChar <|> noneOf "\""))) 
    where
      escapedChar = char '\\' *> char '"'
  numberToken = NUMBER <$> ( many $ oneOf "1234567890" )
  identifierToken = IDENTIFIER <$> ((noneOf "\"'(){}[]1234567890") >> (many (noneOf "\"'(){}[]")))

singleToken :: Parsec String () LoxToken
singleToken =  keywordToken <|> ambiguousToken <|> basicToken <|> literalToken

tokenize :: Parsec String () [LoxToken]
tokenize = many singleToken