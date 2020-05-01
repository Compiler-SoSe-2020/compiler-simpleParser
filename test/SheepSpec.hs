{-# LANGUAGE ScopedTypeVariables #-}

module SheepSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck (Arbitrary, Property, property, (===), (==>))
import qualified Test.QuickCheck as QC (arbitrary, elements, listOf)

import           SheepNoise

-- | Wrapper Typ für [String], damit nur die gewünschten Strings der Form "baa"
-- und "wuff" erzeugt werden.
newtype AnimalNoise = AnimalNoise [String]
  deriving(Show, Eq)

instance Arbitrary AnimalNoise where
  arbitrary = do
    wordList <- (QC.listOf (QC.elements ["baa", "wuff"]))
    pure (AnimalNoise wordList)

prop_baaAccept :: AnimalNoise -> Bool
prop_baaAccept (AnimalNoise noise) =
  let noiseSentence = foldl (++) [] noise
      result = sheepNoise <$> scanner noiseSentence in
    case result of
      Right (Right SN) -> True
      _                -> False

isBaa :: String -> Bool
isBaa "baa" = True
isBaa _     = False

spec :: Spec
spec = do
  describe "`Baa` Tokens" $
    it "should be accepted as SheepNoise" $
    property $
    \(AnimalNoise a) ->
      let baaFiltered = filter isBaa a in
        (baaFiltered /= []) ==> prop_baaAccept (AnimalNoise baaFiltered)

  describe "no tokens" $
    it "should not be accepted as SheepNoise" $
    (sheepNoise <$> scanner []) `shouldBe` Right (Left Nothing)

  describe "mixed Tokens" $
    it "should not be accepted as SheepNoise" $
    property $
    \(AnimalNoise a) ->
        (any (not . isBaa) a) ==> not (prop_baaAccept (AnimalNoise a))

  describe "wrong words" $
    it "should not be scanned correctly" $
    property $
    \(words :: [String]) -> let wordList = foldl (++) [] words
                                scanned = scanner (wordList ++ "!") in
                              case scanned of
                                Left _  -> True
                                Right _ -> False
