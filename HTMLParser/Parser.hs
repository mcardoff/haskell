module Parser where
import Data.List
import Data.Char
import Data.Functor
import Control.Applicative

data HTML = Element {elemName :: String, elemContent :: [HTML]}
          | Attribute {attribName :: String, attribVal :: String}
          | Text {textContent :: String} deriving Show

newtype Parser a = Parser {parse :: String -> Maybe (String, a)}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> do
                          (s', a) <- p s
                          Just (s', f a)

instance Applicative Parser where
    pure x = Parser $ \s -> Just (s,x)
    (Parser pf) <*> (Parser px) =
        Parser $ \s -> do
          (s', f) <- pf s
          (s'',x) <- px s'
          Just (s'',f x)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser a) <|> (Parser b) = Parser $ \s -> a s <|> b s

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \s -> case p s of
                                        Just (s', x) -> parse (f x) s'
                                        _ -> Nothing


charP :: Char -> Parser Char
charP c = Parser $ \s -> case s of
                        (x:xs) | x == c -> Just (xs,x)
                        _ -> Nothing

stringP :: String -> Parser String
stringP = traverse charP

stringLiteral :: Parser String
stringLiteral = betweenP (charP '"') (charP '"') (spanP (/= '"'))

spanP :: (Char -> Bool) -> Parser String
spanP p = Parser $ \s -> let (b,a) = span p s in Just (a,b)

spanP' :: (Char -> Bool) -> Parser String
spanP' p = Parser $ \s -> let (b,a) = span p s in
                          case b of
                            [] -> Nothing
                            _ -> Just (a,b)

betweenP :: Parser a -> Parser b -> Parser c -> Parser c
betweenP l r p = l *> p <* r

ws :: Parser String
ws = spanP isSpace

btwSpace :: Parser a -> Parser a
btwSpace = betweenP ws ws

sepBy :: Parser a -> Parser b -> Parser [b] -- Not used int HTML
sepBy sep elem = ((:) <$> elem <*> many (sep *> elem)) <|> pure []

token :: Parser String
token = betweenP ws ws $ spanP isLetter

openTag :: Parser String
openTag = ws *> charP '<' *> token

anyOpen = ws *> charP '<' *> spanP isLetter <* charP '>' <* ws

closeTag :: String -> Parser String
closeTag s = ws *> stringP "</" *> stringP s <* ws <* charP '>'

anyClose :: Parser String
anyClose = ws *> stringP "</" *> spanP isLetter <* ws <* charP '>'

manyClose :: Parser [String]
manyClose = many anyClose

htmlOpening :: Parser String
htmlOpening = stringP "<!DOCTYPE html>" $> ""

-- Monadic Parsing (gross)

htmlElem :: Parser HTML
htmlElem = do
  name <- openTag
  attribs <- many attribP
  btwSpace $ charP '>'
  content <- many $ htmlElem <|> htmlText
  ws
  closeTag name
  return $ Element name (attribs++content)

htmlAttrib :: Parser HTML
htmlAttrib = do
  name <- token
  charP '='
  val <- stringLiteral
  return $ Attribute name val

htmlText :: Parser HTML
htmlText = textP -- Same as applicative

htmlP :: Parser HTML
htmlP = htmlOpening *> htmlElem

-- Applicative Parsing

attribP :: Parser HTML
attribP = Attribute <$> (token <* charP '=') <*> stringLiteral

textP :: Parser HTML
textP = Text <$> betweenP ws ws (spanP' (\x -> (x/='<')&&(x/='>')))

elemP :: Parser HTML
elemP = (\x y z -> Element x $ y++z)
        <$> openTag
        <*> many attribP <* charP '>'
        <*> many (elemP <|> textP) <* manyClose

html :: Parser HTML
html = htmlOpening *> elemP

htmlDoc2 = "<!DOCTYPE html>\
           \<html>\
           \  <head></head>\
           \  <body>\
           \    <div>\
           \    </div>\
           \  </body>\
           \</html>"

htmlDoc3 = "<!DOCTYPE html>\
           \<html lang=\"en\">\
           \  <head>\
           \    <title>CS 340</title>\
           \  </head>\
           \  <body>\
           \    <div id=\"content\">\
           \      <p>Hello world!</p>\
           \    </div>\
           \  </body>\
           \</html>"

htmlDoc4 = "<!DOCTYPE html>\
           \<html lang=\"en\">\
           \  <head>\
           \    <title>CS 340</title>\
           \  </head>\
           \  <body>\
           \    <div id=\"content\" class=\"row\">\
           \      <p>This is a <strong>cool</strong> paragraph.</p>\
           \      <p>\
           \        This one is <span style=\"font-family: Helvetica;\">\
           \        not so cool.</span>\
           \      </p>\
           \    </div>\
           \    <div id=\"footer\">\
           \      <p>&copy; 2020</p>\
           \    </div>\
           \  </body>\
           \</html>"

execParser :: Parser a -> String -> a
execParser p s = case parse p s of
                   Just ("", y) -> y
                   Just (x , y) -> error "Incomplete Parse"
                   Nothing -> error "Failed Parse"
