{-# LANGUAGE QuasiQuotes, OverloadedStrings, ScopedTypeVariables #-}

module Test.Network.AuthorizeNet.Api (authorizeNetTests) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Data.Aeson
import Text.RawString.QQ

import Network.AuthorizeNet.Api

import Test.Tasty
import Test.Tasty.HUnit

testMerchantAuthentication :: MerchantAuthentication
testMerchantAuthentication = MerchantAuthentication {
  merchantAuthentication_name           = "API_LOGIN_ID",
  merchantAuthentication_transactionKey = "API_TRANSACTION_KEY"
  }

assertEncodes :: (FromJSON a, ToJSON a) => String -> a -> Assertion
assertEncodes expectedS actual = do
  let expectedBsRaw = TL.encodeUtf8 $ TL.pack $ expectedS
      eExpected = (`asTypeOf` actual) <$> eitherDecode expectedBsRaw
      eExpectedBs = encode <$> eExpected :: Either String BSL.ByteString
      actualBs = encode actual
  case eExpectedBs of
    Left e -> error $ show e
    Right expectedBs -> assertEqual "" expectedBs actualBs

-- | These unit tests were created from the examples at http://developer.authorize.net/api/reference/index.html

apiExpected_authenticateTestRequest :: String
apiExpected_authenticateTestRequest = [r|
{
    "authenticateTestRequest": {
        "merchantAuthentication": {
            "name": "API_LOGIN_ID",
            "transactionKey": "API_TRANSACTION_KEY"
        }
    }  
}
|]

apiActual_authenticateTestRequest :: AuthenticateTestRequest
apiActual_authenticateTestRequest = AuthenticateTestRequest testMerchantAuthentication

authorizeNetTests :: TestTree
authorizeNetTests = testGroup "API Requests Encode to JSON" [
  testCase "authenticateTestRequest" $ assertEncodes apiExpected_authenticateTestRequest apiActual_authenticateTestRequest

  ]
