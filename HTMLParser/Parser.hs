module Parser where
import Data.Char
import Data.Functor
import Control.Applicative

-- What is actually gonna be parsed, HTML!

data HTML = Element {elemName :: String, elemContent :: [HTML]}
          | Attribute {attribName :: String, attribVal :: String}
          | Text {textContent :: String} deriving (Show, Eq)

-- Could be expanded further, but this works just fine

newtype Parser s a = Parser {parse :: s -> Maybe (s, a)}

-- instances for Parser, Functor, Applicative, Alternative, and Monad

instance Functor (Parser s) where
    fmap f (Parser p) = Parser $ \s -> do
                          l <- p s
                          Just $ f <$> l

instance Applicative (Parser s) where
    pure x = Parser $ \s -> Just (s,x)
    (Parser pf) <*> (Parser px) =
        Parser $ \s -> do
          (s', f) <- pf s
          l  <- px s'
          Just $ f <$> l

instance Alternative (Parser s) where
    empty = Parser $ const Nothing
    (Parser a) <|> (Parser b) = Parser $ \s -> a s <|> b s

instance Monad (Parser s) where
    return = pure
    (Parser p) >>= f = Parser $
                       \s -> case p s of
                               Just (s', x) -> parse (f x) s'
                               _ -> Nothing

-- General Parser functions --

-- Parses the given character
charP :: Char -> Parser String Char
charP c = Parser $ \s -> case s of
                        (x:xs) | x == c -> Just (xs,x)
                        _ -> Nothing

-- Parses the given string as a sequence of character parsers
stringP :: String -> Parser String String
stringP = traverse charP


-- Parses an identifier between quotes
stringLiteral :: Parser String String
stringLiteral = betweenC '"' '"' (spanP (/= '"'))

-- Same as span for regular strings
spanP :: (Char -> Bool) -> Parser String String
spanP p = Parser $ \s -> let (b,a) = span p s in Just (a,b)

-- Same as before but fails on empty
spanP' :: (Char -> Bool) -> Parser String String
spanP' p = Parser $ \s -> let (b,a) = span p s in
                          case b of
                            [] -> Nothing
                            _ -> Just (a,b)

-- Parses the third parser in between the first two
betweenA :: Parser d a -> Parser d b -> Parser d c -> Parser d c
betweenA l r p = l *> p <* r

betweenM :: Parser d a -> Parser d b -> Parser d c -> Parser d c
betweenM l r p = l >> p << r
    where (<<) = flip (>>)

-- Between, but specifically for chars
betweenC :: Char -> Char -> Parser String a -> Parser String a
betweenC l r = betweenA (charP l) (charP r)

-- Parses something between whitespace
btws :: Parser String a -> Parser String a
btws = betweenA ws ws

-- Parses all ws
ws :: Parser String String
ws = spanP isSpace

-- Parses elem separated by sep
sepBy :: Parser s a -> Parser s b -> Parser s [b] -- Not used in HTML
sepBy sep el = ((:) <$> el <*> many (sep *> el)) <|> pure []

anyOf :: [Parser s a] -> Parser s a
anyOf [] = empty
anyOf ((Parser p):xs) = Parser $ \s -> case p s of
                                         Nothing -> parse (anyOf xs) s
                                         l -> l

-- Same as stringLiteral but uses ws
token :: Parser String String
token = btws $ (spanP' (pred) <|> (many $ anyOf $ map charP "!-"))
  where pred a = (isLetter a) || (isNumber a) 

-- HTML Utility Funcs --

-- parses open tag, but not closing brace
openTag :: Parser String String
openTag = ws *> charP '<' *> token

-- Parses open tag with no attributes
anyOpen :: Parser String String
anyOpen = ws *> charP '<' *> token <* charP '>' <* ws

-- Parses closing tag given a name for the tag
closeTag :: String -> Parser String String
closeTag s = ws *> stringP "</" *> stringP s <* ws <* charP '>'

-- Parses any closing tag
anyClose :: Parser String String
anyClose = ws *> stringP "</" *> spanP isLetter <* ws <* charP '>'

-- Parses many anyClose, defaults to empty
manyClose :: Parser String [String]
manyClose = many anyClose

-- Parses the Beginning of the html with doctype in it
htmlOpening :: Parser String String
htmlOpening = stringP "<!DOCTYPE html>" $> ""

-- Monadic Parsing (gross) --

-- Parses an element, ensures proper HTML formatting
htmlElem :: Parser String HTML
htmlElem = do
  name <- openTag
  attribs <- many attribP
  btws $ charP '>'
  content <- many $ htmlElem <|> htmlText
  ws
  closeTag name
  return $ Element name (attribs++content)

-- Parses attributes of an element
htmlAttrib :: Parser String HTML
htmlAttrib = do
  name <- token
  charP '='
  val <- stringLiteral
  return $ Attribute name val

-- Parses text inside an element
htmlText :: Parser String HTML
htmlText = textP -- Same as applicative

-- Puts it all together
htmlP :: Parser String HTML
htmlP = (many htmlOpening) *> htmlElem

-- Applicative Parsing --

-- Parses Element recursively
-- This works because the way the applicative instance is defined only uses
-- a one-to-one correspondance, so we can take care of the list elements
-- recursively, the only problem with this is that it has no way to ensure
-- that the HTML is properly formatted with respect to closing tags
elemP :: Parser String HTML
elemP = (\x y z -> Element x $ y++z)
        <$> openTag
        <*> many attribP <* charP '>'
        <*> many (elemP <|> textP) <* manyClose

-- Parses attribute of an element
attribP :: Parser String HTML
attribP = Attribute <$> (token <* charP '=') <*> stringLiteral

-- Parses text content of an element
textP :: Parser String HTML
textP = Text <$> betweenA ws ws (spanP' (\x -> (x/='<')&&(x/='>')))

-- Shoves it all together
html :: Parser String HTML
html = (many htmlOpening) *> elemP

-- Examples --
htmlDoc1 :: String
htmlDoc1 = "<html>\
           \  <head>WOAH</head>\
           \  <body>\
           \    <div>\
           \    </div>\
           \  </body>\
           \</html>"

htmlDoc2 :: String
htmlDoc2 = "<!DOCTYPE html>\
           \<html>\
           \  <head></head>\
           \  <body>\
           \    <div>\
           \    </div>\
           \  </body>\
           \</html>"

htmlDoc3 :: String
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

htmlDoc4 :: String
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

htmlTest :: String
htmlTest = "<!DOCTYPE html>\n<html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">\n  \n\n  \n  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1, shrink-to-fit=no\">\n  <style data-tippy-stylesheet=\"\">.tippy-tooltip[data-animation=fade][data-state=hidden]{opacity:0}.tippy-iOS{cursor:pointer!important;-webkit-tap-highlight-color:transparent}.tippy-popper{pointer-events:none;max-width:calc(100vw - 10px);transition-timing-function:cubic-bezier(.165,.84,.44,1);transition-property:transform}.tippy-tooltip{position:relative;color:#fff;border-radius:4px;font-size:14px;line-height:1.4;background-color:#333;transition-property:visibility,opacity,transform;outline:0}.tippy-tooltip[data-placement^=top]>.tippy-arrow{border-width:8px 8px 0;border-top-color:#333;margin:0 3px;transform-origin:50% 0;bottom:-7px}.tippy-tooltip[data-placement^=bottom]>.tippy-arrow{border-width:0 8px 8px;border-bottom-color:#333;margin:0 3px;transform-origin:50% 7px;top:-7px}.tippy-tooltip[data-placement^=left]>.tippy-arrow{border-width:8px 0 8px 8px;border-left-color:#333;margin:3px 0;transform-origin:0 50%;right:-7px}.tippy-tooltip[data-placement^=right]>.tippy-arrow{border-width:8px 8px 8px 0;border-right-color:#333;margin:3px 0;transform-origin:7px 50%;left:-7px}.tippy-tooltip[data-interactive][data-state=visible]{pointer-events:auto}.tippy-tooltip[data-inertia][data-state=visible]{transition-timing-function:cubic-bezier(.54,1.5,.38,1.11)}.tippy-arrow{position:absolute;border-color:transparent;border-style:solid}.tippy-content{padding:5px 9px}</style><link rel=\"icon\" href=\"https://mathshistory.st-andrews.ac.uk/static/img/favicon.gif\">\n\n  <title>5th August - Mathematicians Of The Day - MacTutor History of Mathematics</title>\n\n  <!-- Bootstrap CSS -->\n  <link rel=\"stylesheet\" href=\"./5th August - Mathematicians Of The Day - MacTutor History of Mathematics_files/bootstrap.min.css\">\n\n  <!-- Custom CSS - must be in this order -->\n  <link rel=\"stylesheet\" href=\"./5th August - Mathematicians Of The Day - MacTutor History of Mathematics_files/lato.css\">\n  <link rel=\"stylesheet\" href=\"./5th August - Mathematicians Of The Day - MacTutor History of Mathematics_files/reset.css\">\n  <link rel=\"stylesheet\" href=\"./5th August - Mathematicians Of The Day - MacTutor History of Mathematics_files/custom.css\">\n  <link rel=\"stylesheet\" href=\"./5th August - Mathematicians Of The Day - MacTutor History of Mathematics_files/site.css\">\n  <link rel=\"stylesheet\" href=\"./5th August - Mathematicians Of The Day - MacTutor History of Mathematics_files/content.css\">\n  <link rel=\"stylesheet\" href=\"./5th August - Mathematicians Of The Day - MacTutor History of Mathematics_files/katex.min.css\">\n  <link rel=\"stylesheet\" href=\"./5th August - Mathematicians Of The Day - MacTutor History of Mathematics_files/tooltip.css\">\n\n  \n  \n\n  <!-- opposite of noscript -->\n  <noscript>\n    <style>\n      .nonoscript {\n        display: none;\n      }\n    </style>\n  </noscript>\n  \n<!-- opengraph -->\n<meta property=\"og:title\" content=\"5th August\">\n<meta property=\"og:description\" content=\"Mathematicians that were born or died on 5th August, including a quote from Niels Abel\">\n<meta property=\"og:site_name\" content=\"Maths History\">\n<meta property=\"og:locale\" content=\"en_GB\">\n<meta property=\"og:url\" content=\"https://mathshistory.st-andrews.ac.uk/OfTheDay/oftheday-08-05/\">\n\n\n\n<meta property=\"og:image\" content=\"https://mathshistory.st-andrews.ac.uk/Biographies/Abel/thumbnail.jpg\">\n<meta property=\"og:image:width\" content=\"147\">\n<meta property=\"og:image:height\" content=\"180\">\n<meta property=\"og:image:type\" content=\"image/\">\n\n\n\n<!-- twitter card -->\n<meta name=\"twitter:card\" content=\"summary\">\n<meta name=\"twitter:site\" content=\"Maths History\">\n<meta name=\"twitter:title\" content=\"5th August\">\n<meta name=\"twitter:description\" content=\"Mathematicians that were born or died on 5th August, including a quote from Niels Abel\">\n\n<meta name=\"twitter:image\" content=\"https://mathshistory.st-andrews.ac.uk/Biographies/Abel/thumbnail.jpg\">\n\n\n</head>\n\n<body>\n  <main role=\"main\" class=\"container\">\n\n    \n    <!--noindex-->\n\n    <!-- HEADER -->\n    <header class=\"site-header py-3\">\n      <div class=\"row flex-nowrap justify-content-between align-items-center\">\n        <div class=\"col pt-1\">\n          <a class=\"site-header-name\" href=\"https://mathshistory.st-andrews.ac.uk/\">\n            <h1 class=\"site-header-name\"><img height=\"48\" src=\"./5th August - Mathematicians Of The Day - MacTutor History of Mathematics_files/logo.png\">&nbsp;MacTutor</h1>\n          </a>\n        </div>\n      </div>\n    </header>\n    <!-- END HEADER -->\n\n    <!-- NAVIGATION -->\n    <div class=\"container nav-bar\">\n      <div class=\"row nav\">\n        \n          <a class=\"col-md-auto p-2\" href=\"https://mathshistory.st-andrews.ac.uk/\">Home</a>\n        \n          <a class=\"col-md-auto p-2\" href=\"https://mathshistory.st-andrews.ac.uk/Biographies/\">Biographies</a>\n        \n          <a class=\"col-md-auto p-2\" href=\"https://mathshistory.st-andrews.ac.uk/HistTopics/\">History Topics</a>\n        \n          <a class=\"col-md-auto p-2\" href=\"https://mathshistory.st-andrews.ac.uk/Map/\">Map</a>\n        \n          <a class=\"col-md-auto p-2\" href=\"https://mathshistory.st-andrews.ac.uk/Curves/\">Curves</a>\n        \n          <a class=\"col-md-auto p-2\" href=\"https://mathshistory.st-andrews.ac.uk/Search/\">Search</a>\n        \n      </div>\n    </div>\n    <!-- END NAVIGATION -->\n\n    <!--endnoindex-->\n\n    \n\n<div class=\"row\">\n  <div class=\"col-md-12\">\n    <h2>Mathematicians Of The Day</h2>\n    <h3>5th August</h3>\n  </div>\n</div>\n\n<hr>\n\n<!--noindex-->\n<div class=\"row project-navigation\">\n  <div class=\"col-md-3\">\n    <a href=\"https://mathshistory.st-andrews.ac.uk/OfTheDay/oftheday-08-04/\">Previous Day</a>\n  </div>\n\n  <div class=\"col-md-3\">\n    <a href=\"https://mathshistory.st-andrews.ac.uk/OfTheDay/today/\">Today</a>\n  </div>\n\n  <div class=\"col-md-3\">\n    <a href=\"https://mathshistory.st-andrews.ac.uk/OfTheDay/\">Whole Year</a>\n  </div>\n\n  <div class=\"col-md-3\">\n    <a href=\"https://mathshistory.st-andrews.ac.uk/OfTheDay/oftheday-08-06/\">Next Day</a>\n  </div>\n</div>\n<!--endnoindex-->\n\n<hr>\n\n<div class=\"row\">\n  <div class=\"col-md-6\">\n    <h5>Born:</h5>\n    <ul>\n    \n      <li>\n        1802:\n        <a href=\"https://mathshistory.st-andrews.ac.uk/Biographies/Abel/\">Niels Abel</a>\n        \n        (<a href=\"https://mathshistory.st-andrews.ac.uk/Biographies/Abel/poster/born/\">poster</a>)\n        \n      </li>\n    \n      <li>\n        1855:\n        <a href=\"https://mathshistory.st-andrews.ac.uk/Biographies/Capelli/\">Alfredo Capelli</a>\n        \n      </li>\n    \n      <li>\n        1878:\n        <a href=\"https://mathshistory.st-andrews.ac.uk/Biographies/Karpinski/\">Louis Karpinski</a>\n        \n        (<a href=\"https://mathshistory.st-andrews.ac.uk/Biographies/Karpinski/poster/born/\">poster</a>)\n        \n      </li>\n    \n      <li>\n        1889:\n        <a href=\"https://mathshistory.st-andrews.ac.uk/Biographies/Hamburger/\">Hans Hamburger</a>\n        \n        (<a href=\"https://mathshistory.st-andrews.ac.uk/Biographies/Hamburger/poster/born/\">poster</a>)\n        \n      </li>\n    \n      <li>\n        1936:\n        <a href=\"https://mathshistory.st-andrews.ac.uk/Biographies/Kim/\">Ki Hang Kim</a>\n        \n      </li>\n    \n    </ul>\n  </div>\n\n  <div class=\"col-md-6\">\n    <h5>Died:</h5>\n    <ul>\n    \n      <li>\n        1853:\n        <a href=\"https://mathshistory.st-andrews.pac.uk/Biographies/Olivier/\">Th\233odore Olivier</a>\n        \n        (<a href=\"https://mathshistory.st-andrews.ac.uk/Biographies/Olivier/poster/died/\">poster</a>)\n        \n      </li>\n    \n      <li>\n        1872:\n        <a href=\"https://mathshistory.st-andrews.ac.uk/Biographies/Delaunay/\">Charles Eug\232ne Delaunay</a>\n        \n        (<a href=\"https://mathshistory.st-andrews.ac.uk/Biographies/Delaunay/poster/died/\">poster</a>)\n        \n      </li>\n    \n      <li>\n        1910:\n        <a href=\"https://mathshistory.st-andrews.ac.uk/Biographies/Petersen/\">Julius Petersen</a>\n        \n        (<a href=\"https://mathshistory.st-andrews.ac.uk/Biographies/Petersen/poster/died/\">poster</a>)\n        \n      </li>\n    \n      <li>\n        1981:\n        <a href=\"https://mathshistory.st-andrews.ac.uk/Biographies/Neyman/\">Jerzy Neyman</a>\n        \n        (<a href=\"https://mathshistory.st-andrews.ac.uk/Biographies/Neyman/poster/died/\">poster</a>)\n        \n      </li>\n    \n    </ul>\n  </div>\n</div>\n\n\n\n<hr>\n<div class=\"row\">\n  <div class=\"col-md-12\">\n    <h4>Quotation of the day</h4>\n    <h5>From <a href=\"https://mathshistory.st-andrews.ac.uk/Biographies/Abel/\">Niels Abel</a></h5>\n  </div>\n</div>\n\n<div class=\"row\">\n  <div class=\"col-md-8\">\n    <blockquote>\n  The mathematicians have been very much absorbed with finding the general solution of algebraic equations, and several of them have tried to prove the impossibility of it. However, if I am not mistaken, they have not as yet succeeded. I therefore dare hope that the mathematicians will receive this memoir with good will, for its purpose is to fill this gap in the theory of algebraic equations.<br>\n  \n  <footer class=\"blockquote-footer\">Opening of <i>Memoir on algebraic equations, proving the impossibility of a solution of the general equation of the fifth degree</i> <span class=\"non-italic\">(1824)</span></footer>\n  \n</blockquote>\n  </div>\n\n  \n  \n  <div class=\"col-md-4 biography-thumbnail-container\">\n    \n    <a href=\"https://mathshistory.st-andrews.ac.uk/Biographies/Abel/pictdisplay/\">\n    \n    <img class=\"biography-thumbnail\" src=\"./5th August - Mathematicians Of The Day - MacTutor History of Mathematics_files/thumbnail.jpg\">\n    \n    <br>\n    View seven larger pictures</a>\n    \n  </div>\n  \n</div>\n\n\n\n<!-- insert theorem here -->\n\n\n\n    \n    <!--noindex-->\n\n    <footer class=\"site-footer mt-3 pb-3 pt-3 pt-md-0\">\n      <div class=\"row\">\n        <div class=\"cr-footer col-md-4\">\n          DJF/JOC/EFR<br>\n          <a href=\"https://mathshistory.st-andrews.ac.uk/Miscellaneous/copyright/\">Copyright information</a>\n        </div>\n        <div class=\"links-footer col-md-6\">\n          <a href=\"http://www.st-andrews.ac.uk/maths/\" target=\"_blank\">School of Mathematics and Statistics</a>\n          <br>\n          <a href=\"http://www.st-andrews.ac.uk/\" target=\"_blank\">University of St Andrews, Scotland</a>\n        </div>\n        <div class=\"col-md-2\">\n          <img src=\"./5th August - Mathematicians Of The Day - MacTutor History of Mathematics_files/st-andrews-logo.png\" alt=\"University of St Andrews logo\">\n        </div>\n      </div>\n      <hr>\n      <div class=\"row\">\n        <div class=\"col-md-12\">\n          If you have comments, or spot errors, we are always pleased to <a href=\"https://mathshistory.st-andrews.ac.uk/Miscellaneous/contact_us/\">hear from you</a>.\n        </div>\n      </div>\n    </footer>\n\n    <!--endnoindex-->\n\n  </main>\n\n  \n  <script src=\"./5th August - Mathematicians Of The Day - MacTutor History of Mathematics_files/popper.min.js\"></script>\n  <script src=\"./5th August - Mathematicians Of The Day - MacTutor History of Mathematics_files/tippy-bundle.iife.min.js\"></script>\n  <script src=\"./5th August - Mathematicians Of The Day - MacTutor History of Mathematics_files/site-popups.js\"></script>\n  <script src=\"./5th August - Mathematicians Of The Day - MacTutor History of Mathematics_files/foldup.js\"></script>\n  \n  <script src=\"./5th August - Mathematicians Of The Day - MacTutor History of Mathematics_files/extras-popup.js\"></script>\n  \n  <script src=\"./5th August - Mathematicians Of The Day - MacTutor History of Mathematics_files/imageMapResizer.min.js\"></script>\n  <script>imageMapResize();</script>\n  \n\n\n\n</body></html>"

getHTML = do content <- readFile "/home/mcard/Downloads/test.html"
             print $ execParser (htmlOpening *> spanP (/='\n') *> ws *> htmlElem)content

-- Use This to test if the parser succedded or failed --
execParser :: Parser String a -> String -> a
execParser p s = case parse p s of
                   Just ("", y) -> y
                   Just (x , y) -> error "Incomplete Parse"
                   Nothing -> error "Failed Parse"
