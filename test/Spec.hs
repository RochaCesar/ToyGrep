import Data.Char
import Data.List
import Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Match Literal Characters" $ do
    it "matches single literal character" $ do
      matchPattern "a" "apple" `shouldBe` True
      matchPattern "a" "dog" `shouldBe` False

    it "literal character matches itself in any string" $
      property $
        \c s ->
          not (null s) ==>
            matchPattern [c] s == (c `elem` s)

  describe "Match Digits" $ do
    it "matches digit pattern" $ do
      matchPattern "\\d" "123" `shouldBe` True
      matchPattern "\\d" "abc" `shouldBe` False

    it "\\d matches any digit" $
      property $
        \n -> matchPattern "\\d" [n] == isDigit n

    it "\\d matches string containing any digits" $
      property $
        \s ->
          not (null s) ==>
            matchPattern "\\d" s == any isDigit s

  describe "Match Alphanumeric Characters" $ do
    it "matches alphanumeric pattern" $ do
      matchPattern "\\w" "foo101" `shouldBe` True
      matchPattern "\\w" "$!?" `shouldBe` False

    it "\\w matches any alphanumeric character" $
      property $
        \c -> matchPattern "\\w" [c] == isAlphaNum c

    it "\\w matches string containing any alphanumeric" $
      property $
        \s ->
          not (null s) ==>
            matchPattern "\\w" s == any isAlphaNum s

  describe "Edge Cases" $ do
    it "handles empty pattern" $
      property $
        \s -> matchPattern "" s == True

    it "handles empty string" $
      property $
        \p -> p /= "" ==> matchPattern p "" == False

    it "matches exact strings" $
      property $
        \s -> not (null s) ==> matchPattern s s == True

  describe "Multiple Character Patterns" $ do
    it "handles literal multi-char patterns" $ do
      matchPattern "abc" "abcdef" `shouldBe` True
      matchPattern "abc" "acbdef" `shouldBe` False

    it "matches consecutive patterns" $
      property $
        \s ->
          length s >= 3 ==>
            matchPattern (take 3 s) s == True

  describe "Special Cases" $ do
    it "handles escaped characters" $ do
      matchPattern "\\\\d" "\\d" `shouldBe` True
      matchPattern "\\\\w" "\\w" `shouldBe` True

    let specialChars = "\\[]{}()*+?.|^$"
    it "matches escaped special characters" $
      property $
        forAll (elements specialChars) $ \c ->
          matchPattern ['\\', c] [c] == True
