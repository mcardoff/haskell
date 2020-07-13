module Parser where
import Data.List
import Data.Char
import Data.Functor
import Control.Applicative

-- What is actually gonna be parsed, HTML!

data HTML = Element {elemName :: String, elemContent :: [HTML]}
          | Attribute {attribName :: String, attribVal :: String}
          | Text {textContent :: String} deriving Show

-- Could be expanded further, but this works just fine

newtype Parser a = Parser {parse :: String -> Maybe (String, a)}

-- instances for Parser, Functor, Applicative, Alternative, and Monad

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

-- General Parser functions --

-- Parses the given character
charP :: Char -> Parser Char
charP c = Parser $ \s -> case s of
                        (x:xs) | x == c -> Just (xs,x)
                        _ -> Nothing

-- Parses the given string as a sequence of character parseres
stringP :: String -> Parser String
stringP = traverse charP


-- Parses an identifier between quotes
stringLiteral :: Parser String
stringLiteral = betweenC '"' '"' (spanP (/= '"'))

-- Same as span for regular strings
spanP :: (Char -> Bool) -> Parser String
spanP p = Parser $ \s -> let (b,a) = span p s in Just (a,b)

-- Same as before but fails on empty
spanP' :: (Char -> Bool) -> Parser String
spanP' p = Parser $ \s -> let (b,a) = span p s in
                          case b of
                            [] -> Nothing
                            _ -> Just (a,b)

-- Parses the third parser in between the first two
betweenP :: Parser a -> Parser b -> Parser c -> Parser c
betweenP l r p = l *> p <* r

-- Between, but specifically for chars
betweenC :: Char -> Char -> Parser a -> Parser a
betweenC l r = betweenP (charP l) (charP r)

-- Parses something between whitespace
btws :: Parser a -> Parser a
btws = betweenP ws ws

-- Parses all ws
ws :: Parser String
ws = spanP isSpace

-- Parses elem separated by sep
sepBy :: Parser a -> Parser b -> Parser [b] -- Not used in HTML
sepBy sep elem = ((:) <$> elem <*> many (sep *> elem)) <|> pure []

-- Same as stringLiteral but uses ws
token :: Parser String
token = btws $ spanP isLetter

-- HTML Utility Funcs --

-- parses open tag, but not closing brace
openTag :: Parser String
openTag = ws *> charP '<' *> token

-- Parses open tag with no attributes
anyOpen :: Parser String
anyOpen = ws *> charP '<' *> spanP isLetter <* charP '>' <* ws

-- Parses closing tag given a name for the tag
closeTag :: String -> Parser String
closeTag s = ws *> stringP "</" *> stringP s <* ws <* charP '>'

-- Parses any closing tag
anyClose :: Parser String
anyClose = ws *> stringP "</" *> spanP isLetter <* ws <* charP '>'

-- Parses many anyClose, defaults to empty
manyClose :: Parser [String]
manyClose = many anyClose

-- Parses the Beginning of the html with doctype in it
htmlOpening :: Parser String
htmlOpening = stringP "<!DOCTYPE html>" $> ""

-- Monadic Parsing (gross) --

-- Parses an element, ensures proper HTML formatting
htmlElem :: Parser HTML
htmlElem = do
  name <- openTag
  attribs <- many attribP
  btws $ charP '>'
  content <- many $ htmlElem <|> htmlText
  ws
  closeTag name
  return $ Element name (attribs++content)

-- Parses attributes of an element
htmlAttrib :: Parser HTML
htmlAttrib = do
  name <- token
  charP '='
  val <- stringLiteral
  return $ Attribute name val

-- Parses text inside an element
htmlText :: Parser HTML
htmlText = textP -- Same as applicative

-- Puts it all together
htmlP :: Parser HTML
htmlP = htmlOpening *> htmlElem

-- Applicative Parsing --

-- Parses Element recursively
elemP :: Parser HTML
elemP = (\x y z -> Element x $ y++z)
        <$> openTag
        <*> many attribP <* charP '>'
        <*> many (elemP <|> textP) <* manyClose

-- Parses attribute of an element
attribP :: Parser HTML
attribP = Attribute <$> (token <* charP '=') <*> stringLiteral

-- Parses text content of an element
textP :: Parser HTML
textP = Text <$> betweenP ws ws (spanP' (\x -> (x/='<')&&(x/='>')))

-- Shoves it all together
html :: Parser HTML
html = htmlOpening *> elemP

-- Examples --
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

-- Use This to test if the parser succedded or failed --
execParser :: Parser a -> String -> a
execParser p s = case parse p s of
                   Just ("", y) -> y
                   Just (x , y) -> error "Incomplete Parse"
                   Nothing -> error "Failed Parse"
