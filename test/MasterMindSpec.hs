module MasterMindSpec (main, spec) where
	
	import Test.Hspec
	import Test.QuickCheck
	
	import MasterMind
	
	-- `main` is here so that this module can be run from GHCi on its own.  It is
	-- not needed for automatic spec discovery.
	main :: IO ()
	main = hspec spec
	
	spec :: Spec
	spec = do
		describe "markGuess" $ do
			it "All wrong" $ do markGuess "1111" "0000" `shouldBe` ""
			it "1 correct" $ do markGuess "1234" "1000" `shouldBe` "*"
			it "2 correct" $ do markGuess "1234" "1200" `shouldBe` "**"
			it "2 ends correct" $ do markGuess "1234" "1004" `shouldBe` "**"
			it "3 correct" $ do markGuess "1234" "1230" `shouldBe` "***"
			it "4 correct" $ do markGuess "1234" "1234" `shouldBe` "****"
			it "1 wrong place" $ do markGuess "1234" "0001" `shouldBe` "."
			it "2 wrong place" $ do markGuess "1234" "4001" `shouldBe` ".."
			it "1 full 1 partial" $ do markGuess "1234" "1002" `shouldBe` "*."
			it "2 full 2 partial" $ do markGuess "1234" "1324" `shouldBe` "**.."