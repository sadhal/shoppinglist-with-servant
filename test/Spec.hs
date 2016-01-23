
import LibFetch (filterContent)
import Test.Hspec

main :: IO ()
--main = putStrLn "Test suite not yet implemented"
main = hspec $ do
  describe "LibFetch.filterContent" $ do
    it "returns empty string when substring not found in content" $ do
      filterContent "abc" "dab" `shouldBe` ""

    it "returns whole content when substring is found" $ do
      filterContent "abc" "dab cab abcsdga" `shouldBe` "dab cab abcsdga"
