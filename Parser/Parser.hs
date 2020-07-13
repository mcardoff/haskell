module Parser where
import Data.Char
import Control.Applicative

newtype Parser s a = Parser {parse :: s -> Maybe (s, a)}



instance Functor (Parser s) where
    fmap f (Parser a) = Parser $ \s -> case a s of
                                         Nothing -> Nothing
                                         Just (s', x) -> Just (s', f x)

instance Applicative (Parser s) where
    pure x = Parser $ \s -> Just (s,x)
    (Parser fs) <*> (Parser ns) = Parser $ \s ->
        do (s',f) <- fs s
           (s'',x) <- ns s'
           return (s'', f x)

instance Alternative (Parser s) where
    empty = Parser $ const Nothing
    (Parser a) <|> (Parser b) = Parser $ \s -> a s <|> b s

anyChar :: Parser String Char
anyChar = Parser $ \s -> case s of
                           [] -> Nothing
                           (x:xs) -> Just (xs,x)


char :: Char -> Parser String Char
char c = predicate (==c)

string :: String -> Parser String String
string s = sequenceA $ map char s

predicate :: (Char -> Bool) -> Parser String Char
predicate p = Parser $ \s -> if (not $ null s) && (p $ head s)
                             then Just (tail s, head s)
                             else Nothing

spanP :: (Char -> Bool) -> Parser String String
spanP p = Parser $ \s -> let (a, b) = span p s in Just (b, a)

ws :: Parser String String
ws = spanP isSpace

digit :: Parser String Intx
digit = read <$> spanP isDigit

between :: Parser s a -> Parser s b -> Parser s c -> Parser s c
between l r c = l *> c <* r

------------- Calculator Parser -------------
data Expr = Lit Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
            deriving Show

evaluate :: Expr -> Int
evaluate c = case c of
               Lit x -> x
               Add e1 e2 -> evaluate e1 + evaluate e2
               Sub e1 e2 -> evaluate e1 - evaluate e2
               Mul e1 e2 -> evaluate e1 * evaluate e2

numP :: Parser Int
numP = read <$> spanP isDigit

exprP :: Parser Expr
exprP = addP <|> subP <|> termP

termP :: Parser Expr
termP = mulP <|> factorP

factorP :: Parser Expr
factorP = litP <|> parens exprP

litP :: Parser Expr
litP = Lit <$> numP

addP :: Parser Expr
addP = Add <$> termP <*> (spaceSep '+' *> exprP)

subP :: Parser Expr
subP = Sub <$> termP <*> (spaceSep '-' *> exprP)

mulP :: Parser Expr
mulP = Mul <$> factorP <*> (spaceSep '*' *> termP)

spaceSep :: Char -> Parser Char
spaceSep c = betweenP ws ws $ charP c

parens :: Parser a -> Parser a
parens = betweenP (charP '(') (charP ')')
