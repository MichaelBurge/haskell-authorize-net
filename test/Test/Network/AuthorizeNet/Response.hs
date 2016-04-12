{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Test.Network.AuthorizeNet.Response (responseTests) where

import Network.AuthorizeNet.Api
import Network.AuthorizeNet.Response

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Data.Aeson
import Data.Monoid
import Text.RawString.QQ

import Test.Tasty
import Test.Tasty.HUnit

import Test.Network.AuthorizeNet.Util

testMessages :: Messages
testMessages = Messages Message_Ok $ [ Message "I00001" "Successful." ]

apiExpected_authenticateTestResponse :: String
apiExpected_authenticateTestResponse = [r|
{
    "messages": {
        "resultCode": "Ok",
        "message": [
            {
                "code": "I00001",
                "text": "Successful."
            }
        ]
    }
}
|]

apiActual_authenticateTestResponse :: AuthenticateTest
apiActual_authenticateTestResponse = mkAuthenticateTest testMessages

apiExpected_createCustomerProfileResponse :: String
apiExpected_createCustomerProfileResponse = [r|
{
    "customerProfileId": "190178",
    "customerPaymentProfileIdList": [
        "157497"
    ],
    "customerShippingAddressIdList": [],
    "validationDirectResponseList": [
        "1,1,1,(TESTMODE) This transaction has been approved.,000000,P,0,none,Test transaction for ValidateCustomerPaymentProfile.,1.00,CC,auth_only,Merchant_Customer_ID,,,,,,,,,,,customer-profile-email@her.com,,,,,,,,,0.00,0.00,0.00,FALSE,none,207BCBBF78E85CF174C87AE286B472D2,,,,,,,,,,,,,XXXX1111,Visa,,,,,,,,,,,,,,,,"
    ],
    "messages": {
        "resultCode": "Ok",
        "message": [
            {
                "code": "I00001",
                "text": "Successful."
            }
        ]
    }
}
|]

apiActual_createCustomerProfileResponse :: CreateCustomerProfile
apiActual_createCustomerProfileResponse = CreateCustomerProfile {
  createCustomerProfile_refId = Nothing,                         
  createCustomerProfile_messages = testMessages,              
  createCustomerProfile_sessionToken = Nothing,      
  createCustomerProfile_customerProfileId = Just 190178,             
  createCustomerProfile_customerPaymentProfileIdList = [157497], 
  createCustomerProfile_customerShippingAddressIdList = [],
  createCustomerProfile_validationDirectResponseList = [ "1,1,1,(TESTMODE) This transaction has been approved.,000000,P,0,none,Test transaction for ValidateCustomerPaymentProfile.,1.00,CC,auth_only,Merchant_Customer_ID,,,,,,,,,,,customer-profile-email@her.com,,,,,,,,,0.00,0.00,0.00,FALSE,none,207BCBBF78E85CF174C87AE286B472D2,,,,,,,,,,,,,XXXX1111,Visa,,,,,,,,,,,,,,,," ]
  }


responseTests :: TestTree
responseTests = testGroup "API Responses Encode and Decode to JSON correctly" [
      testCase "authenticateTestResponse" $ assertEncodes apiExpected_authenticateTestResponse apiActual_authenticateTestResponse,
      testCase "createCustomerProfileResponse" $ assertEncodes apiExpected_createCustomerProfileResponse apiActual_createCustomerProfileResponse
      ]
