module Scanner where
import Text.ParserCombinators.Parsec

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

