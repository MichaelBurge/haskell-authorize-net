{-# LANGUAGE QuasiQuotes, OverloadedStrings, ScopedTypeVariables #-}

module Test.Network.AuthorizeNet.Api (authorizeNetTests) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Data.Aeson
import Data.Monoid
import Text.RawString.QQ

import Network.AuthorizeNet.Api

import Test.Tasty
import Test.Tasty.HUnit

testMerchantAuthentication :: MerchantAuthentication
testMerchantAuthentication = MerchantAuthentication {
  merchantAuthentication_name           = "API_LOGIN_ID",
  merchantAuthentication_transactionKey = "API_TRANSACTION_KEY"
  }

testCustomerProfile :: CustomerProfile
testCustomerProfile = CustomerProfile {
  customer_profileId          = Nothing,
  customer_merchantCustomerId = "Merchant_Customer_ID",
  customer_description        = "Profile description here",
  customer_email              = "customer-profile-email@here.com",
  customer_paymentProfiles    = Just $ CustomerPaymentProfile {
      customerPaymentProfile_customerType = Just CustomerType_individual,
      customerPaymentProfile_billTo = Nothing,
      customerPaymentProfile_driversLicense = Nothing,
      customerPaymentProfile_taxId = Nothing,
      customerPaymentProfile_payment = Just $ Payment_creditCard CreditCard {
          creditCard_cardNumber = "4111111111111111",
          creditCard_expirationDate = "2020-12",
          creditCard_cardCode = Nothing
          }
      },
  customer_shipTos            = Nothing
  }

assertEncodes :: (FromJSON a, ToJSON a) => String -> a -> Assertion
assertEncodes expectedS actual = do
  let expectedBsRaw = TL.encodeUtf8 $ TL.pack $ expectedS
      eExpected = (`asTypeOf` actual) <$> eitherDecode expectedBsRaw
      eExpectedBs = encode <$> eExpected :: Either String BSL.ByteString
      actualBs = encode actual
  case eExpectedBs of
    Left e -> error $ "Error parsing '" <> expectedS <> "': " <> show e
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

apiActual_authenticateTestRequest :: ApiRequest
apiActual_authenticateTestRequest = AuthenticateTest testMerchantAuthentication

apiExpected_createCustomerProfileRequest :: String
apiExpected_createCustomerProfileRequest = [r|
{
    "createCustomerProfileRequest": {
        "merchantAuthentication": {
            "name": "API_LOGIN_ID",
            "transactionKey": "API_TRANSACTION_KEY"
        },
        "profile": {
            "merchantCustomerId": "Merchant_Customer_ID",
            "description": "Profile description here",
            "email": "customer-profile-email@here.com",
            "paymentProfiles": {
                "customerType": "individual",
                "payment": {
                    "creditCard": {
                        "cardNumber": "4111111111111111",
                        "expirationDate": "2020-12"
                    }
                }
            }
        },
        "validationMode": "testMode"
    }
}
|]

apiActual_createCustomerProfileRequest :: ApiRequest
apiActual_createCustomerProfileRequest = CreateCustomerProfile testMerchantAuthentication testCustomerProfile

apiExpected_getCustomerProfileRequest :: String
apiExpected_getCustomerProfileRequest = [r|
{
    "getCustomerProfileRequest": {
        "merchantAuthentication": {
            "name": "API_LOGIN_ID",
            "transactionKey": "API_TRANSACTION_KEY"
        },
        "customerProfileId": "10000"
    }
}
|]

apiActual_getCustomerProfileRequest :: ApiRequest
apiActual_getCustomerProfileRequest = GetCustomerProfile testMerchantAuthentication "10000"

authorizeNetTests :: TestTree
authorizeNetTests = testGroup "API Requests Encode to JSON" [
  testCase "authenticateTestRequest" $ assertEncodes apiExpected_authenticateTestRequest apiActual_authenticateTestRequest,
  testCase "createCustomerProfileRequest" $ assertEncodes apiExpected_createCustomerProfileRequest apiActual_createCustomerProfileRequest,
  testCase "getCustomerProfileRequest" $ assertEncodes apiExpected_getCustomerProfileRequest apiActual_getCustomerProfileRequest
  ]
