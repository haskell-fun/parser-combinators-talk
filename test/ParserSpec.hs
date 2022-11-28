module ParserSpec where

import Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

spec :: Spec
spec = describe "Simple test" $ do

     it "parse True" $
        runParser bool "True"  `shouldBe` Just True 

     it "parse False" $
        runParser bool "False"  `shouldBe` Just False 
     

     it "fail to parse abc as boolean" $
        runParser bool "abs"  `shouldBe` Nothing
     
     it "parse hello" $
        runParser (string "hello") "hello" `shouldBe` Just "hello"


     it "fail to parse abc" $
        runParser (string "hello") "abc" `shouldBe` Nothing



     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])

