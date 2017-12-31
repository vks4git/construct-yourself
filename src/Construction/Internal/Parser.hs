module Construction.Internal.Parser where

import           Construction.Internal.Types (Term (..), Type (..))
import           Data.Text                   (pack)
import qualified Data.Text                   as DT (null)
import           Text.Parsec                 (getInput)
import           Text.Parsec.Char            (char, digit, letter, space,
                                              string)
import           Text.Parsec.Combinator      (between, many1)
import           Text.Parsec.Prim            (many, try, (<|>))
import           Text.Parsec.Text            (Parser)

greedy :: Parser a -> Parser a
greedy basicP = do
  parsedObj <- basicP
  input     <- getInput
  if DT.null input then pure parsedObj else fail "Parses did not greedy all input stream."

typeP :: Parser Type
typeP = tvarP <|> tarrP

tvarP :: Parser Type
tvarP = (\x y -> TVar $ pack (x:y)) <$> letter
                                   <*> many digit

tarrP :: Parser Type
tarrP = try $ between (char '(') (char ')') $ do
  t1 <- typeP
  _  <- many space >> string "->" >> many space
  t2 <- typeP
  pure $ TArr t1 t2


termP :: Parser Term
termP = varP <|> appP <|> lamP <|> bracketP

varP :: Parser Term
varP =  (\x y -> Var $ pack (x:y)) <$> letter
                                   <*> many digit

appP :: Parser Term
appP = try $ between (char '(') (char ')') $
       App <$> (termP <* many1 space)
           <*> termP

bracketP :: Parser Term
bracketP = between (char '(') (char ')') termP

lamP :: Parser Term
lamP = do
  char '\\'
  varName <- var <$> varP
  char '.'
  body <- termP
  pure $ Lam varName body

nameP :: Parser String
nameP = (:) <$> char 'x' <*> many digit
