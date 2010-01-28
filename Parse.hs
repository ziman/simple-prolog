module Parse (term, query, clause, program) where

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.Char
import Data.List
import Control.Monad
import Control.Applicative hiding (many, (<|>))

import Term

query :: Parser Query
query = list term <* kwd "." <* eof <?> "query"

program :: Parser Program
program = sp *> many clause <* eof <?> "program"

clause :: Parser Clause
clause = Clause <$> func <*> termList <?> "clause"

termList :: Parser [LTerm]
termList = (kwd "." >> return []) <|> (kwd ":-" *> list term <* kwd ".") <?> "term list"

term :: Parser LTerm
term = func <|> var <?> "term"

var :: Parser LTerm
var = Var <$> variable <?> "variable"

func :: Parser LTerm
func = Fun <$> atom <*> (maybe [] id <$> optionMaybe (parens $ list term)) <?> "function/predicate"

list :: Parser a -> Parser [a]
list = (`sepBy` kwd ",")

kwd :: String -> Parser ()
kwd s = string s >> sp <?> s

parens :: Parser a -> Parser a
parens = between (kwd "(") (kwd ")")

variable :: Parser Name
variable = ((:) <$> upper <*> many letter) <* sp <?> "variable"

atom :: Parser Name
atom = ((:) <$> lower <*> many letter) <* sp <?> "atom"

sp :: Parser ()
sp = many ((satisfy isSpace >> return ()) <|> comment) >> return () <?> "whitespace or comment"

comment :: Parser ()
comment = char '%' >> many (satisfy (/= '\n')) >> return () <?> "comment"
