{-# LANGUAGE DeriveFunctor #-}

import Data.Char
import Data.Maybe
import Data.Monoid
import Control.Monad

type ProgramaFuente = String

type Palabra = String

type Posicion = (Int, Int)

data Posicionado a = Pos Posicion a
                     deriving (Show, Functor)

data Lexema = Identificador String
            | LitNumEntero Int
            | PalabraClave PalabraClave
            deriving (Show)

data PalabraClave = PCIf
                  | PCElse
                  | PCWhile
                  deriving (Show)

type Reconocedor = Palabra -> Maybe Lexema

separador :: ProgramaFuente -> [Posicionado Palabra]
separador pf = do
    (i, ln) <- lineasNum pf
    ncs <- palabrasNum ln
    let j = fst $ head ncs
    let w = map snd ncs
    return $ Pos (i, j) w
  where
    lineasNum :: String -> [(Int, String)]
    lineasNum = zip [1..] . lines
    
    palabrasNum :: String -> [[(Int, Char)]]
    palabrasNum = go . zip [1..]
      where
        go s =
          case dropWhile (isSpace . snd) s of
            [] -> []
            s' -> w : go s''
                  where
                    (w, s'') = break (isSpace . snd) s'

identificador :: Reconocedor
identificador p@(c:cs) = do
  guard $ c `elem` charsIdFst
  guard $ all (flip elem charsId) cs
  return $ Identificador p
  where
    charsIdFst = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
    charsId = charsIdFst ++ ['0'..'9']

litNumEntero :: Reconocedor
litNumEntero p = do
  guard $ all isDigit p
  return $ LitNumEntero $ read p

palabraClave :: Lexema -> String -> Reconocedor
palabraClave lx pc p = if p == pc then Just lx else Nothing

palabrasClave :: [Reconocedor]
palabrasClave = map (uncurry $ palabraClave . PalabraClave) pcs
  where
    pcs = [(PCIf, "if")]

lexemizador :: [Reconocedor] -> [Posicionado Palabra] -> Either String [Posicionado Lexema]
lexemizador rs = mapM reconocer
  where
    reconocer :: Posicionado Palabra -> Either String (Posicionado Lexema)
    reconocer (Pos pos p) =
      case listToMaybe $ mapMaybe ($ p) rs of
        Nothing -> Left ("Error lex: " ++ show pos)
        Just lx -> Right (Pos pos lx)

lexemizar = lexemizador rs . separador
  where
    rs = palabrasClave ++ [identificador, litNumEntero]

main = do
  pf <- getLine
  print $ lexemizar pf
