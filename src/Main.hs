import Language.Python.Parser.Lexer as L

main :: IO ()
main = do
  file <- getContents
  print $ L.lex "<stdin>" file
