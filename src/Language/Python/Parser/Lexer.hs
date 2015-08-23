module Language.Python.Parser.Lexer
  ( PositionedToken(..)
  , Token()
  , lex
  )
  where

import Prelude hiding (lex)

import Data.Char (isSpace, digitToInt)

import Data.Maybe (maybeToList)
import Control.Monad (void, guard, when)
import Data.Functor.Identity

import Control.Applicative

import Language.Python.Parser.State

import Text.Parsec ((<?>))

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as PT

-- https://github.com/python/cpython/blob/master/Include/token.h
data Token
  = EndMarker
  | NewLine
  | Indent
  | Dedent
  
  | LParen
  | RParen
  | LSquare
  | RSquare
  | LBrace
  | RBrace
  
  | Colon
  | Comma
  | Semi
  | Dot
  | Ellipsis
  
  | Tilde
  | Plus
  | Minus
  | Star
  | Slash
  | VBar
  | Amper
  | Less
  | Greater
  | Equal
  | Percent
  | Circumflex
  | LShift
  | RShift
  | DoubleStar
  | DoubleSlash
  | At
  
  | EqEqual
  | NotEqual
  | LessEqual
  | GreaterEqual
  
  | PlusEqual
  | MinusEqual
  | StarEqual
  | SlashEqual
  | VBarEqual
  | AmperEqual
  | PercentEqual
  | CircumflexEqual
  | LShiftEqual
  | RShiftEqual
  | DoubleStarEqual
  | DoubleSlashEqual
  | AtEqual
  
  | Name String
  | Number Number
  | StringLiteral String
  deriving (Show, Eq, Ord)

data Number
  = IntLiteral Integer
  | FloatLiteral Double
  | ImaginaryLiteral Double
  deriving (Show, Eq, Ord)

data PositionedToken = PositionedToken
  { ptSourcePos :: P.SourcePos
  , ptToken     :: Token
  } deriving (Eq)

instance Show PositionedToken where
  show = show . ptToken

lex :: FilePath -> String -> Either P.ParseError [PositionedToken]
lex filePath input = P.runParser parseTokens def filePath input
  where
    def = LexParseState
      { lpsOpenBraces = 0
      , lpsIndentLevels = [0]
      }

data LexParseState = LexParseState
  { lpsOpenBraces   :: !Int -- (, [, {
  , lpsIndentLevels :: [Int]
  }

type LexemeParser a = P.Parsec String LexParseState a

getOpenBraces :: LexemeParser Int
getOpenBraces = lpsOpenBraces <$> P.getState

putOpenBraces :: Int -> LexemeParser ()
putOpenBraces n = P.modifyState (\s -> s { lpsOpenBraces = n })

modifyOpenBraces :: (Int -> Int) -> LexemeParser ()
modifyOpenBraces f = putOpenBraces . f =<< getOpenBraces

incOpenBraces :: LexemeParser ()
incOpenBraces = modifyOpenBraces succ

decOpenBraces :: LexemeParser ()
decOpenBraces = modifyOpenBraces pred

getIndentLevels :: LexemeParser [Int]
getIndentLevels = lpsIndentLevels <$> P.getState

putIndentLevels :: [Int] -> LexemeParser ()
putIndentLevels ns = P.modifyState (\s -> s { lpsIndentLevels = ns })

getIndentLevel :: LexemeParser Int
getIndentLevel = head <$> getIndentLevels

pushIndentLevel :: Int -> LexemeParser ()
pushIndentLevel n = putIndentLevels . (n:) =<< getIndentLevels

whitespace :: LexemeParser ()
whitespace = P.skipMany (P.char ' ')

parseTokens :: LexemeParser [PositionedToken]
parseTokens = do
  P.skipMany ignorable
  tokens <- concat <$> P.many parseLogicalLine
  P.eof
  endIndents <- length . filter (>0) <$> getIndentLevels
  dedents <- mapM position $ replicate endIndents Dedent
  endMarker <- position EndMarker
  return (tokens ++ dedents ++ [endMarker])

parseIndentation :: LexemeParser [PositionedToken]
parseIndentation = mapM position =<< go
  where
    go = do
      curr <- length <$> P.many (P.char ' ')
      prev <- getIndentLevel
      case curr `compare` prev of
        EQ -> return []
        GT -> return [Indent] <* pushIndentLevel curr
        LT -> do
          levels <- getIndentLevels
          when (not $ elem curr levels) (fail $ "indentation must be: " ++ show levels)
          let (pop, levels') = span (> curr) levels
          putIndentLevels levels'
          return $ replicate (length pop) Dedent

emptyLine :: LexemeParser ()
emptyLine = void (whitespace *> P.char '\n')

eol :: LexemeParser ()
eol = void (P.char '\n') P.<?> "newline"

backslash = P.char '\\'

position :: Token -> LexemeParser PositionedToken
position t = do
  pos <- P.getPosition
  return $ PositionedToken pos t

parseComment :: LexemeParser ()
parseComment = (void $ lineComment) P.<?> "comment"
  where
  lineComment :: LexemeParser String
  lineComment = P.string "#" *> P.manyTill P.anyChar (P.try eol)

ignorable = P.try emptyLine <|> P.try (whitespace *> parseComment)

parseLogicalLine :: LexemeParser [PositionedToken]
parseLogicalLine = do
  P.skipMany ignorable
  indentation <- parseIndentation
  (indentation ++) <$> begin
  where
    begin = do
      tokens <- P.many1 parsePositionedToken
      let continue = (tokens ++) <$> begin
      
      implicitJoin <- (> 0) <$> getOpenBraces
      if implicitJoin then do
        whitespace
        parseComment <|> (P.optional backslash *> eol)
        P.skipMany ignorable
        whitespace
        continue
      else do
        whitespace
        explicitJoin <- P.optionMaybe (backslash *> eol)
        case explicitJoin of
          Nothing   -> ((\t -> tokens ++ [t]) <$> position NewLine) <* P.optional eol
          otherwise -> whitespace *> continue

parsePositionedToken :: LexemeParser PositionedToken
parsePositionedToken = P.try $ position =<< parseToken

parseToken :: LexemeParser Token
parseToken = P.choice
  [ Name          <$> parseName
  , Number        <$> P.try number
  , StringLiteral <$> parseString
  
  , P.try $ P.char '('      *> incOpenBraces *> pure LParen
  , P.try $ P.char ')'      *> decOpenBraces *> pure RParen
  , P.try $ P.char '['      *> incOpenBraces *> pure LSquare
  , P.try $ P.char ']'      *> decOpenBraces *> pure RSquare
  , P.try $ P.char '{'      *> incOpenBraces *> pure LBrace
  , P.try $ P.char '}'      *> decOpenBraces *> pure RBrace
  
  , P.try $ P.char ':'      *> pure Colon
  , P.try $ P.char ','      *> pure Comma
  , P.try $ P.char ';'      *> pure Semi
  , P.try $ P.string "..."  *> pure Ellipsis
  , P.try $ P.char '.'      *> pure Dot
  
  , P.try $ P.char '~'      *> pure Tilde
  , P.try $ P.char '+'      *> pure Plus
  , P.try $ P.char '-'      *> pure Minus
  , P.try $ P.char '*'      *> pure Star
  , P.try $ P.char '/'      *> pure Slash
  , P.try $ P.char '|'      *> pure VBar
  , P.try $ P.char '&'      *> pure Amper
  , P.try $ P.char '<'      *> pure Less
  , P.try $ P.char '>'      *> pure Greater
  , P.try $ P.char '='      *> pure Equal
  , P.try $ P.char '%'      *> pure Percent
  , P.try $ P.char '^'      *> pure Circumflex
  , P.try $ P.string "<<"   *> pure LShift
  , P.try $ P.string ">>"   *> pure RShift
  , P.try $ P.string "**"   *> pure DoubleStar
  , P.try $ P.string "//"   *> pure DoubleSlash
  , P.try $ P.char '@'      *> pure At
  
  , P.try $ P.string "=="   *> pure EqEqual
  , P.try $ P.string "!="   *> pure NotEqual
  , P.try $ P.string "<="   *> pure LessEqual
  , P.try $ P.string ">="   *> pure GreaterEqual
  
  , P.try $ P.string "+="   *> pure PlusEqual
  , P.try $ P.string "-="   *> pure MinusEqual
  , P.try $ P.string "*="   *> pure StarEqual
  , P.try $ P.string "/="   *> pure SlashEqual
  , P.try $ P.string "|="   *> pure VBarEqual
  , P.try $ P.string "&="   *> pure AmperEqual
  , P.try $ P.string "<="   *> pure LessEqual
  , P.try $ P.string ">="   *> pure GreaterEqual
  , P.try $ P.string "%="   *> pure PercentEqual
  , P.try $ P.string "^="   *> pure CircumflexEqual
  , P.try $ P.string "<<="  *> pure LShiftEqual
  , P.try $ P.string ">>="  *> pure RShiftEqual
  , P.try $ P.string "**="  *> pure DoubleStarEqual
  , P.try $ P.string "//="  *> pure DoubleSlashEqual
  , P.try $ P.string "@="   *> pure At
  ] <* whitespace
  
  where
  -----------------------------------------------------------
  -- Identifiers
  -----------------------------------------------------------
  
  parseName :: LexemeParser String
  parseName = (:) <$> identStart <*> P.many identLetter
  
  identStart :: LexemeParser Char
  identStart = P.lower <|> P.upper <|> P.oneOf "_"
  
  identLetter :: LexemeParser Char
  identLetter = P.alphaNum <|> P.oneOf "_"
  
  -----------------------------------------------------------
  -- Numbers
  -----------------------------------------------------------
  
  number :: LexemeParser Number
  number =  do{ n <- intFloatNumber
              ; P.option n (imaginaryTag n)
              }
  
  intFloatNumber :: LexemeParser Number
  intFloatNumber =    (P.char '0' *> zeroNumber)
                  <|> fractNumber
                  <|> decNumber
  
  zeroNumber :: LexemeParser Number
  zeroNumber =  IntLiteral <$> (hexInteger <|> octInteger <|> binInteger)
            <|> (FloatLiteral <$> fractExp 0)
            <|> decNumber
            <|> return (IntLiteral 0)
  
  decNumber :: LexemeParser Number
  decNumber =  do{ n <- decInteger
                 ; P.option (IntLiteral n) (FloatLiteral <$> fractExp n)
                 }
  
  fractNumber :: LexemeParser Number
  fractNumber = do{ P.char '.'
                  ; f <- fraction
                  ; e <- P.option 1.0 exponent'
                  ; return (FloatLiteral (f * e))
                  }
  
  fractExp :: Integer -> LexemeParser Double
  fractExp n =  do{ P.char '.'
                  ; f <- P.option 0.0 fraction
                  ; e <- P.option 1.0 exponent'
                  ; return ((fromInteger n + f) * e)
                  }
              <|>
                do{ e <- exponent'
                  ; return ((fromInteger n) * e)
                  }
  
  fraction :: LexemeParser Double
  fraction = do{ digits <- P.many1 P.digit <?> "fraction"
               ; return (foldr op 0.0 digits)
               }
             <?> "fraction"
            where
              op d f = (f + fromIntegral (digitToInt d))/10.0
  
  exponent' :: LexemeParser Double
  exponent' = do{ P.oneOf "eE"
                ; f <- sign
                ; e <- decInteger <?> "exponent"
                ; return (power (f e))
                }
              <?> "exponent"
            where
               power e  | e < 0      = 1.0/power(-e)
                        | otherwise  = fromInteger (10^e)
  
  imaginaryTag :: Number -> LexemeParser Number
  imaginaryTag n = P.char 'j' *> return (toIm n)
                where
                  toIm (IntLiteral i)   = ImaginaryLiteral (fromInteger i)
                  toIm (FloatLiteral f) = ImaginaryLiteral f
  
  sign :: Num a => LexemeParser (a -> a)
  sign  =   (P.char '-' >> return negate)
        <|> (P.char '+' >> return id)
        <|> return id
  
  decInteger = baseInteger 10 P.digit
  hexInteger = P.oneOf "xX" *> baseInteger 16 P.hexDigit
  octInteger = P.oneOf "oO" *> baseInteger 8 P.octDigit
  binInteger = P.oneOf "bB" *> baseInteger 2 (P.oneOf "01")
  
  baseInteger :: Integer -> LexemeParser Char -> LexemeParser Integer
  baseInteger base baseDigit
    = do{ digits <- P.many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }
  
  -----------------------------------------------------------
  -- Strings
  -----------------------------------------------------------
  
  parseString :: LexemeParser String
  parseString = blockSingle <|> blockDouble <|> single <|> double
    where
      blockSingle = parseStringLit "\"\"\"" blockChar
      blockDouble = parseStringLit "\'\'\'" blockChar
      single      = parseStringLit "\'" singleChar
      double      = parseStringLit "\"" singleChar
      blockChar   = P.anyChar
      singleChar  = P.noneOf "\n"
  
  parseStringLit :: String -> LexemeParser Char -> LexemeParser String
  parseStringLit delim cp = delimiter *> (concat <$> P.manyTill character delimiter)
    where
      delimiter = P.try (P.string delim)
      character = escapeSeq <|> ((:[]) <$> cp)
  
  escapeSeq :: LexemeParser String
  escapeSeq = P.try $ do
    P.char '\\'
    e <- P.oneOf "\\'\"abfnrtv\n"
    return $ represent e
    where
    represent e =
      case e of
        '\\'  -> "\\"
        '\''  -> "\'"
        '"'   -> "\""
        'a'   -> "\a"
        'b'   -> "\b"
        'f'   -> "\f"
        'n'   -> "\n"
        'r'   -> "\r"
        't'   -> "\t"
        'v'   -> "\v"
        '\n'  -> []
