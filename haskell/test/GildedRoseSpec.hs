module GildedRoseSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import GildedRose

spec :: Spec
spec = do
  describe "normal items" $ do

    it "decreases days" $ property $
      \sellIn quality ->
        let inventory = Item "foo" sellIn quality
            (Item _ sellIn' _) = updateQualityItem inventory
        in sellIn' === sellIn - 1

    it "decreases quality when greater than 0" $ property $
      \(Positive sellIn) (Positive quality) ->
        let inventory = Item "foo" sellIn quality
            (Item _ _ quality') = updateQualityItem inventory
        in quality' === max (quality - 1) 0

    it "decreases quality twice as fast when after sellIn" $ property $
      \(Positive sellIn) (Positive quality) ->
        let inventory = Item "foo" (-sellIn) quality
            (Item _ _ quality') = updateQualityItem inventory
        in quality' === max (quality - 2) 0

    it "quality is never negative" $ property $
      \sellIn (Positive quality) ->
        let inventory = Item "foo" sellIn quality
            (Item _ _ quality') = updateQualityItem inventory
        in quality' >= 0

    it "quality is never over 50" $ property $
      \sellIn quality ->
        let inventory = Item "foo" sellIn quality
            (Item _ _ quality') = updateQualityItem inventory
        in quality <= 50 ==> quality' <= 50

  describe "Aged Brie" $ do

    it "decreases days" $ property $
      \sellIn quality ->
        let inventory = Item "Aged Brie" sellIn quality
            (Item _ sellIn' _) = updateQualityItem inventory
        in sellIn' === sellIn - 1

    it "increases quality when greater than 0" $ property $
      \(Positive sellIn) (Positive quality) ->
        let inventory = Item "Aged Brie" sellIn quality
            (Item _ _ quality') = updateQualityItem inventory
        in quality <= 50 ==> quality' === min (quality + 1) 50

    it "increases quality twice as fast when after sellIn" $ property $
      \sellIn quality ->
        let inventory = Item "Aged Brie" sellIn quality
            (Item _ _ quality') = updateQualityItem inventory
        in sellIn <= 0 && quality <= 50 ==> quality' === min (quality + 2) 50

    it "quality is never negative" $ property $
      \sellIn (Positive quality) ->
        let inventory = Item "Aged Brie" sellIn quality
            (Item _ _ quality') = updateQualityItem inventory
        in quality' >= 0

    it "quality is never over 50" $ property $
      \sellIn quality ->
        let inventory = Item "Aged Brie" sellIn quality
            (Item _ _ quality') = updateQualityItem inventory
        in quality <= 50 ==> quality' <= 50

  describe "Sulfuras, Hand of Ragnaros" $ do

    it "doesn't change" $ property $
      \sellIn quality ->
        let inventory = Item "Sulfuras, Hand of Ragnaros" sellIn quality
            (Item _ sellIn' quality') = updateQualityItem inventory
        in sellIn' === sellIn .&&. quality' === quality

  describe "Backstage passes to a TAFKAL80ETC concert" $ do

    it "decreases days" $ property $
      \sellIn quality ->
        let inventory = Item "Backstage passes to a TAFKAL80ETC concert" sellIn quality
            (Item _ sellIn' _) = updateQualityItem inventory
        in sellIn' === (sellIn - 1)

    it "increases quality when more than 10 days left" $ property $
      \(Positive sellIn) (Positive quality) ->
        let inventory = Item "Backstage passes to a TAFKAL80ETC concert" sellIn quality
            (Item _ _ quality') = updateQualityItem inventory
        in sellIn > 10 && quality <= 50 ==> quality' === min (quality + 1) 50

    it "increases quality by 2 when less than 10 and more than 5 days left" $
      forAll (choose (6, 10)) $
        \sellIn (Positive quality) ->
          let inventory = Item "Backstage passes to a TAFKAL80ETC concert" sellIn quality
              (Item _ _ quality') = updateQualityItem inventory
          in quality <= 50 ==> quality' === min (quality + 2) 50

    it "increases quality by 3 when less than 5 days left" $
      forAll (choose (1, 5)) $
        \sellIn (Positive quality) ->
          let inventory = Item "Backstage passes to a TAFKAL80ETC concert" sellIn quality
              (Item _ _ quality') = updateQualityItem inventory
              in quality <= 50 ==> quality' === min (quality + 3) 50

    it "sets quality to 0 after sellIn" $ property $
      \(Positive sellIn) (Positive quality) ->
        let inventory = Item "Backstage passes to a TAFKAL80ETC concert" (-sellIn) quality
            (Item _ _ quality') = updateQualityItem inventory
        in quality' === 0
