module Parser where
import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser s a = Parser {parse :: s -> Maybe (s,a)}

instance Functor (Parser s) where
    -- Invade a function with a function, apply it to the 'a'
    fmap f (Parser s) = Parser $ s >=> (\x -> return $ f <$> x)


instance Applicative (Parser s) where
    -- Easiest way to put a function in a box
    pure x = Parser $ \input -> Just (input, x)
    (Parser fs) <*> (Parser xs) = Parser $ \input ->
        do
          (input', f) <- fs input
          (input'', x) <- xs input'
          Just (input'',f x)

instance Alternative (Parser s) where
    empty = Parser $ const Nothing
    (Parser a) <|> (Parser b) = Parser $ \input -> a input <|> b input

instance Monad (Parser s) where
    return = pure
    (Parser p) >>= f = Parser $ \s ->
                          case p s of
                            Nothing -> Nothing
                            Just (s', x) -> parse (f x) s'

-- Give it a character and then it will check to see if that character is in the string
char :: Char -> Parser String Char
char c = Parser $ \s -> case s of
                   (x:xs) | x == c -> Just (xs,x)
                   _ -> Nothing

-- Logic is that we can sequentially apply the char on each element of the string
string :: String -> Parser String String
string = traverse char

predP :: (s -> Bool) -> Parser [s] s
predP p = Parser $ \s -> case s of
                          (x:xs) | p x -> Just (xs,x)
                          _ -> Nothing

-- (*>) and (<*) are sequencers, and ignores the left and right args respectively
between :: Parser s a -> Parser s b -> Parser s c -> Parser s c
between l r m = l *> m <* r

betweenS :: Parser s a -> Parser s c -> Parser s c
betweenS s = between s s

parens :: Parser String a -> Parser String a
parens = between (char '(') (char ')')

spanP :: (p -> Bool) -> Parser [p] [p]
spanP p = Parser $ \input -> let (a,b) = span p input in Just (b,a)

ws :: Parser String String
ws = spanP isSpace

token :: String -> Parser String String
token s = string s <* ws

chainl1 :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl1 p op = undefined

sepBy :: Parser s a   -- Parser for the separators
      -> Parser s b   -- Parser for elements
      -> Parser s [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

chainOp :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainOp p op = p >>= rest
    where rest a = (do f <- op
                       b <- p
                       rest (f a b)) <|> return a
-- EOF
