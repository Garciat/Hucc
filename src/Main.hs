import Language.Python.Parser.Lexer as L

main = do
  file <- getContents
  print $ L.lex "<stdin>" file
