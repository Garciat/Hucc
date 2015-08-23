import Test.Hspec
import Data.Either (isLeft)
import qualified Language.Python.Parser.Lexer as P

main :: IO ()
main = hspec $ do
  describe "Language.Python.Parser.Lexer.lex" $ do
    it "parses Python numeric literals" $ do
      "0"     `lexesTo` [P.Number (P.IntLiteral 0)]
      "00000" `lexesTo` [P.Number (P.IntLiteral 0)]
      
      "0."    `lexesTo` [P.Number (P.FloatLiteral 0.0)]
      ".0"    `lexesTo` [P.Number (P.FloatLiteral 0.0)]
      "0.0"   `lexesTo` [P.Number (P.FloatLiteral 0.0)]
      
      "1"     `lexesTo` [P.Number (P.IntLiteral 1)]
      "1."    `lexesTo` [P.Number (P.FloatLiteral 1.0)]
      "1.0"   `lexesTo` [P.Number (P.FloatLiteral 1.0)]
      "1.e0"  `lexesTo` [P.Number (P.FloatLiteral 1.0)]
      
    
    it "does not parse invalid Python numeric literals" $ do
      lexFailsFor "01"
  
  where
    lexFailsFor input =
      P.lex "<spec>" input `shouldSatisfy` isLeft
    
    lexesTo input tokens =
      let result = map P.ptToken <$> P.lex "<spec>" input in
      result `shouldBe` Right (tokens ++ [P.NewLine, P.EndMarker])