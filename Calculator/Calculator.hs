module Calculator where
import Parser
import Data.Char
import Control.Applicative
import Data.Functor

data Expr = Mul Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Lit Int deriving (Show)

eval :: Expr -> Int
eval e = case e of
           Mul a b -> eval a * eval b
           Add a b -> eval a + eval b
           Sub a b -> eval a - eval b
           Lit n   -> n

digit :: Parser String String
digit = spanP isDigit

number :: Parser String Int
number = read <$> ((:) <$> (char '-' <|> pure '0') <*> digit)

lit :: Parser String Expr
lit = Lit <$> number

-- order of operations goes add/sub -> mul -> num

add :: Parser String Expr
add = mul `chainOp` addOp

mul :: Parser String Expr
mul = factor `chainOp` mulOp

factor :: Parser String Expr
factor = lit <|> parens add

infixOp :: String -> (a -> a -> a) -> Parser String (a -> a -> a)
infixOp s f = token s $> f
-- parses the string op, then in its place it will insert the function

addOp :: Parser String (Expr -> Expr -> Expr)
addOp = infixOp "+" Add <|> infixOp "-" Sub

mulOp :: Parser String (Expr -> Expr -> Expr)
mulOp = infixOp "*" Mul

-- EOF
