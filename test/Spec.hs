import Test.Hspec
import Types
import Parser
import Eval
import Control.Monad.State (evalStateT)

main :: IO ()
main = hspec $ do
  describe "parsers work correctly" $ do
    it "parse lit" $ do
      parse "true" `shouldBe` Right (TmBool True)
      parse "false" `shouldBe` Right (TmBool False)
      parse "42" `shouldBe` Right (TmNat 42)
    it "parse lambda" $ do
      parse "(lambda [x: Nat] x)" `shouldBe` Right (TmAbs "x" NatType (TmVar 0))
      parse "(lambda [x: Nat -> Bool -> Nat] x)" `shouldBe` Right (TmAbs "x" (FnType NatType (FnType BoolType NatType)) (TmVar 0))
  describe "integrated test" $ do
    it "eval factorial" $ do
      let program = "(let [fac = (fix \
      \(lambda [self: Nat -> Nat] \
        \(lambda [x: Nat] \
          \(if (= x 0) 1 (* x (self (- x 1)))))))] (fac 10))"
      let result = parse program >>= \term -> evalStateT (typeOf term >> eval term) []
      result `shouldBe` Right (TmNat (foldl (*) 1 [1..10]))
