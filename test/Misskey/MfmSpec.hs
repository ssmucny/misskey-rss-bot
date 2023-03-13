module Misskey.MfmSpec (spec) where

import RIO
import Test.Hspec

import Misskey.Mfm
import Misskey.Types

testRemoteUser = User
    { id = UserId "test-user-id"
    , name = "test user"
    , username = "testuser"
    , host = Just $ Url "remote.io"
    }

testLocalUser = User
    { id = UserId "test-local-id"
    , name = "test user local"
    , username = "testlocal"
    , host = Nothing
    }

spec = do
    describe "mentions" $ do
        it "mention local user" $ mention testLocalUser `shouldBe` "@testlocal"
        it "mention remote user" $ mention testRemoteUser `shouldBe` "@testuser@remote.io"
    describe "hashtag" $ do
        it "empty text" $ hashtag "" `shouldBe` ""
        it "simple tag" $ hashtag "fediverse" `shouldBe` "#fediverse"
        it "multi word tag" $ hashtag "fun with monads" `shouldBe` "#funwithmonads"
        it "conserve case" $ hashtag "downtown CLE" `shouldBe` "#downtownCLE"
    describe "url link" $ do
        it "plain link" $ urlLink (Url "http://thecle.land") Nothing `shouldBe` "http://thecle.land"
        it "link with display text" $ urlLink (Url "https://thecle.land") (Just "The CLE") `shouldBe` "[The CLE](https://thecle.land)"
    describe "emoji" $ do
        it "simple emoji" $ emoji "+1" `shouldBe` ":+1:"