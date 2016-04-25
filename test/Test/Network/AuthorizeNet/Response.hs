{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Test.Network.AuthorizeNet.Response (responseTests) where

import Network.AuthorizeNet.Response
import Network.AuthorizeNet.Types

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Data.Monoid
import Text.RawString.QQ

import Test.Tasty
import Test.Tasty.HUnit

import Test.Network.AuthorizeNet.Util

testMessages :: Messages
testMessages = Messages Message_Ok $ ArrayOf [ Message "I00001" "Successful." ]

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

apiActual_authenticateTestResponse :: AuthenticateTestResponse
apiActual_authenticateTestResponse = mkAuthenticateTestResponse testMessages

-- apiExpected_createCustomerProfileResponse :: String
-- apiExpected_createCustomerProfileResponse = [r|
-- {
--     "customerProfileId": "190178",
--     "customerPaymentProfileIdList": [
--         "157497"
--     ],
--     "customerShippingAddressIdList": [],
--     "validationDirectResponseList": [
--         "1,1,1,(TESTMODE) This transaction has been approved.,000000,P,0,none,Test transaction for ValidateCustomerPaymentProfile.,1.00,CC,auth_only,Merchant_Customer_ID,,,,,,,,,,,customer-profile-email@her.com,,,,,,,,,0.00,0.00,0.00,FALSE,none,207BCBBF78E85CF174C87AE286B472D2,,,,,,,,,,,,,XXXX1111,Visa,,,,,,,,,,,,,,,,"
--     ],
--     "messages": {
--         "resultCode": "Ok",
--         "message": [
--             {
--                 "code": "I00001",
--                 "text": "Successful."
--             }
--         ]
--     }
-- }
-- |]

-- apiActual_createCustomerProfileResponse :: CreateCustomerProfileResponse
-- apiActual_createCustomerProfileResponse = CreateCustomerProfileResponse {
--   createCustomerProfileResponse_refId = Nothing,                         
--   createCustomerProfileResponse_messages = testMessages,              
--   createCustomerProfileResponse_sessionToken = Nothing,      
--   createCustomerProfileResponse_customerProfileId = Just 190178,             
--   createCustomerProfileResponse_customerPaymentProfileIdList = [157497], 
--   createCustomerProfileResponse_customerShippingAddressIdList = [],
--   createCustomerProfileResponse_validationDirectResponseList = [ "1,1,1,(TESTMODE) This transaction has been approved.,000000,P,0,none,Test transaction for ValidateCustomerPaymentProfile.,1.00,CC,auth_only,Merchant_Customer_ID,,,,,,,,,,,customer-profile-email@her.com,,,,,,,,,0.00,0.00,0.00,FALSE,none,207BCBBF78E85CF174C87AE286B472D2,,,,,,,,,,,,,XXXX1111,Visa,,,,,,,,,,,,,,,," ]
--   }


-- apiExpected_getCustomerProfileResponse :: String
-- apiExpected_getCustomerProfileResponse = [r|
-- {
--     "profile": {
--         "paymentProfiles": [
--             {
--                 "customerPaymentProfileId": "35936989",
--                 "payment": {
--                     "creditCard": {
--                         "cardNumber": "XXXX1111",
--                         "expirationDate": "XXXX"
--                     }
--                 },
--                 "customerType": "individual",
--                 "billTo": {
--                     "firstName": "John",
--                     "lastName": "Smith"
--                 }
--             }
--         ],
--         "customerProfileId": "39598611",
--         "merchantCustomerId": "CUST001",
--         "description": "Profile created by Subscription: 3078153",
--         "email": "joe@mail.com"
--     },
--     "subscriptionIds": [
--         "3078153",
--         "3078154"
--     ],
--     "messages": {
--         "resultCode": "Ok",
--         "message": [
--             {
--                 "code": "I00001",
--                 "text": "Successful."
--             }
--         ]
--     }
-- }
-- |]
 
-- test_getCustomerProfileIdsResponse :: Assertion
-- test_getCustomerProfileIdsResponse =
--   let text = [r|
-- {
--     "ids": [
--         "47988",
--         "47997",
--         "48458",
--         "48468",
--         "189118",
--         "190178"
--     ],
--     "messages": {
--         "resultCode": "Ok",
--         "message": [
--             {
--                 "code": "I00001",
--                 "text": "Successful."
--             }
--         ]
--     }
-- }
-- |]
--       response = GetCustomerProfileIdsResponse Nothing testMessages Nothing $ ArrayOf [47988, 47997, 48458, 48468, 189118, 190178 ]
--   in assertEncodes text response

-- test_updateCustomerProfileResponse :: Assertion
-- test_updateCustomerProfileResponse =
--   let text = [r|
-- {
--     "messages": {
--         "resultCode": "Ok",
--         "message": [
--             {
--                 "code": "I00001",
--                 "text": "Successful."
--             }
--         ]
--     }
-- }
-- |]
--       response = UpdateCustomerProfileResponse Nothing testMessages Nothing
--   in assertEncodes text response
-- test_deleteCustomerProfileResponse :: Assertion
-- test_deleteCustomerProfileResponse =
--   let text = [r|
-- {
--     "messages": {
--         "resultCode": "Ok",
--         "message": [
--             {
--                 "code": "I00001",
--                 "text": "Successful."
--             }
--         ]
--     }
-- }
-- |]
--       response = DeleteCustomerProfileResponse Nothing testMessages Nothing
--   in assertEncodes text response

-- test_createCustomerPaymentProfileResponse :: Assertion
-- test_createCustomerPaymentProfileResponse =
--   let text = [r|
-- {
--     "customerPaymentProfileId": "157498",
--     "validationDirectResponse": "1,1,1,This transaction has been approved.,KJE50J,Y,2149186956,none,Test transaction for ValidateCustomerPaymentProfile.,0.00,CC,auth_only,none,John,Doe,,123 Main St.,Bellevue,WA,98004,USA,000-000-0000,,email@example.com,,,,,,,,,0.00,0.00,0.00,FALSE,none,035AD411ACC863BC459E6E24F6C29D95,M,1,,,,,,,,,,,XXXX1111,Visa,,,,,,,,,,,,,,,,",
--     "messages": {
--         "resultCode": "Ok",
--         "message": [
--             {
--                 "code": "I00001",
--                 "text": "Successful."
--             }
--         ]
--     }
-- }
-- |]
--       response = CreateCustomerPaymentProfileResponse Nothing testMessages Nothing (Just 157498) $ Just "1,1,1,This transaction has been approved.,KJE50J,Y,2149186956,none,Test transaction for ValidateCustomerPaymentProfile.,0.00,CC,auth_only,none,John,Doe,,123 Main St.,Bellevue,WA,98004,USA,000-000-0000,,email@example.com,,,,,,,,,0.00,0.00,0.00,FALSE,none,035AD411ACC863BC459E6E24F6C29D95,M,1,,,,,,,,,,,XXXX1111,Visa,,,,,,,,,,,,,,,,"
--   in assertEncodes text response

-- test_getCustomerPaymentProfileResponse :: Assertion
-- test_getCustomerPaymentProfileResponse =
--   let text = [r|
-- {
--     "paymentProfile": {
--         "customerProfileId": "39598611",
--         "customerPaymentProfileId": "35936989",
--         "payment": {
--             "creditCard": {
--                 "cardNumber": "XXXX1111",
--                 "expirationDate": "XXXX"
--             }
--         },
--         "subscriptionIds": [
--             "3078153",
--             "3078154"
--         ],
--         "customerType": "individual",
--         "billTo": {
--             "firstName": "John",
--             "lastName": "Smith"
--         }
--     },
--     "messages": {
--         "resultCode": "Ok",
--         "message": [
--             {
--                 "code": "I00001",
--                "text": "Successful."
--             }
--         ]
--     }
-- }
-- |]
--       response = GetCustomerPaymentProfileResponse Nothing testMessages Nothing $ Just CustomerPaymentProfileMasked {
--         customerPaymentProfileMasked_customerProfileId = Just 39598611,
--         customerPaymentProfileMasked_customerPaymentProfileId = 35936989,
--         customerPaymentProfileMasked_payment = Just $ PaymentMasked_creditCard CreditCardMasked {
--           creditCardMasked_cardNumber = "XXXX1111",
--           creditCardMasked_expirationDate = "XXXX",
--           creditCardMasked_cardType = Nothing,
--           creditCardMasked_cardArt = Nothing
--         },
--         customerPaymentProfileMasked_driversLicense = Nothing,
--         customerPaymentProfileMasked_taxId = Nothing,
--         customerPaymentProfileMasked_subscriptionIds = Just [ 3078153, 3078154 ]
--       }
--   in assertEncodes text response

-- test_getCustomerPaymentProfileListResponse :: Assertion
-- test_getCustomerPaymentProfileListResponse =
--   let text = [r|
-- { 
--          "getCustomerPaymentProfileListResponse": { 
--                  "messages": { 
--                          "resultCode": "Ok", 
--                          "message": { 
--                                 "code": "I00001", 
--                                 "text": "Successful." 
--                          } 
--                 }, 
--                  "totalNumInResultSet": "1", 
--                  "paymentProfiles": { 
--                          "paymentProfile": { 
--                                 "customerPaymentProfileId": "1051079", 
--                                  "customerProfileId": "918787", 
--                                  "billTo": { 
--                                          "firstName": "John", 
--                                          "lastName": "Smith" 
--                                  }, 
--                                  "payment": { 
--                                          "creditCard": { 
--                                                 "cardNumber": "XXXX1111", 
--                                                  "expirationDate": "XXXX" 
--                                          } 
--                                  } 
--                         } 
--                  } 
--         } 
--  }
-- |]
--       response = GetCustomerPaymentProfileListResponse Nothing testMessages Nothing 1 $
--                  Just $ ArrayOfCustomerPaymentProfileListItem $ ArrayOf $ pure $
--                  CustomerPaymentProfileListItem {
--                    customerPaymentProfileListItem_customerPaymentProfileId = 1051079,
--                    customerPaymentProfileListItem_customerProfileId        = 918787,
--                    customerPaymentProfileListItem_billTo                   = mkCustomerAddress {
--                        customerAddress_firstName = Just "John",
--                        customerAddress_lastName = Just "Smith"
--                    },
--                    customerPaymentProfileListItem_payment = PaymentMasked_creditCard $ mkCreditCardMasked "XXXX1111" "XXXX"
--                }
--   in assertEncodes text response

-- test_validateCustomerPaymentProfileResponse :: Assertion
-- test_validateCustomerPaymentProfileResponse =
--   let text = [r|
-- {
--     "directResponse": "1,1,1,This transaction has been approved.,8F14E1,Y,2149186958,none,Test transaction for ValidateCustomerPaymentProfile.,0.00,CC,auth_only,ydidgxugkfsjqpdmxwby,John,Doe,,123 Main St.,Bellevue,WA,98004,USA,000-000-0000,,enitsigjfk@authorize.net,,,,,,,,,0.00,0.00,0.00,FALSE,none,32CB5A012BEC7145759EDC186968351A,M,8,,,,,,,,,,,XXXX1111,Visa,,,,,,,,,,,,,,,,",
--     "messages": {
--         "resultCode": "Ok",
--         "message": [
--             {
--                 "code": "I00001",
--                 "text": "Successful."
--             }
--         ]
--     }
-- }
-- |]
--       response = ValidateCustomerPaymentProfileResponse Nothing testMessages Nothing $ Just "1,1,1,This transaction has been approved.,8F14E1,Y,2149186958,none,Test transaction for ValidateCustomerPaymentProfile.,0.00,CC,auth_only,ydidgxugkfsjqpdmxwby,John,Doe,,123 Main St.,Bellevue,WA,98004,USA,000-000-0000,,enitsigjfk@authorize.net,,,,,,,,,0.00,0.00,0.00,FALSE,none,32CB5A012BEC7145759EDC186968351A,M,8,,,,,,,,,,,XXXX1111,Visa,,,,,,,,,,,,,,,,"
--   in assertEncodes text response

-- test_updateCustomerPaymentProfileResponse :: Assertion
-- test_updateCustomerPaymentProfileResponse =
--   let text = [r|
-- {
--     "validationDirectResponse": "1,1,1,This transaction has been approved.,454ID3,Y,2149186959,none,Test transaction for ValidateCustomerPaymentProfile.,0.00,CC,auth_only,ydidgxugkfsjqpdmxwby,John,Doe,,123 Main St.,Bellevue,WA,98004,USA,000-000-0000,,enitsigjfk@authorize.net,,,,,,,,,0.00,0.00,0.00,FALSE,none,FDD599BEA746A09F7F32F220F402C849,M,8,,,,,,,,,,,XXXX1111,Visa,,,,,,,,,,,,,,,,",
--     "messages": {
--         "resultCode": "Ok",
--         "message": [
--             {
--                 "code": "I00001",
--                 "text": "Successful."
--             }
--         ]
--     }
-- }
-- |]
--       response = UpdateCustomerPaymentProfileResponse Nothing testMessages Nothing $ Just "1,1,1,This transaction has been approved.,454ID3,Y,2149186959,none,Test transaction for ValidateCustomerPaymentProfile.,0.00,CC,auth_only,ydidgxugkfsjqpdmxwby,John,Doe,,123 Main St.,Bellevue,WA,98004,USA,000-000-0000,,enitsigjfk@authorize.net,,,,,,,,,0.00,0.00,0.00,FALSE,none,FDD599BEA746A09F7F32F220F402C849,M,8,,,,,,,,,,,XXXX1111,Visa,,,,,,,,,,,,,,,,"
--   in assertEncodes text response

-- test_deleteCustomerPaymentProfileResponse :: Assertion
-- test_deleteCustomerPaymentProfileResponse =
--   let text = [r|
-- {
--     "messages": {
--         "resultCode": "Ok",
--         "message": [
--             {
--                 "code": "I00001",
--                 "text": "Successful."
--             }
--         ]
--     }
-- }
-- |]
--       response = DeleteCustomerPaymentProfileResponse Nothing testMessages Nothing
--   in assertEncodes text response
-- test_createCustomerProfileFromTransactionResponse :: Assertion
-- test_createCustomerProfileFromTransactionResponse =
--   let text = [r|
-- {
--     "customerProfileId": "190179",
--     "customerPaymentProfileIdList": [
--         "157500"
--     ],
--     "customerShippingAddressIdList": [
--         "126407"
--     ],
--     "validationDirectResponseList": [],
--     "messages": {
--         "resultCode": "Ok",
--         "message": [
--             {
--                 "code": "I00001",
--                 "text": "Successful."
--             }
--         ]
--     }
-- }
-- |]
--       response = CreateCustomerProfileResponse Nothing testMessages Nothing (Just 190179) [ 157500 ] [ 126407 ] []
--   in assertEncodes text response

-- test_chargeCustomerProfile :: Assertion
-- test_chargeCustomerProfile =
--   let text = [r|
-- {
--     "transactionResponse": {
--         "responseCode": "1",
--         "authCode": "C1E3I6",
--         "avsResultCode": "Y",
--         "cvvResultCode": "S",
--         "cavvResultCode": "9",
--         "transId": "2149186775",
--         "refTransID": "",
--         "transHash": "C85B15CED28462974F1114DB07A16C39",
--         "testRequest": "0",
--         "accountNumber": "XXXX0015",
--         "accountType": "MasterCard",
--         "messages": [
--             {
--                 "code": "1",
--                 "description": "This transaction has been approved."
--             }
--         ],
--         "userFields": [
--             {
--                 "name": "MerchantDefinedFieldName1",
--                 "value": "MerchantDefinedFieldValue1"
--             },
--             {
--                 "name": "favorite_color",
--                 "value": "blue"
--             }
--         ]
--     },
--     "refId": "123456",
--     "messages": {
--         "resultCode": "Ok",
--         "message": [
--             {
--                 "code": "I00001",
--                 "text": "Successful."
--             }
--         ]
--     }
-- }
-- |]
--       transactionResponse = mkTransactionResponse {
--         transactionResponse_responseCode = Just "1",
--         transactionResponse_authCode = Just "C1E3I6",
--         transactionResponse_avsResultCode = Just "Y",
--         transactionResponse_cvvResultCode = Just "S",
--         transactionResponse_cavvResultCode = Just "9",
--         transactionResponse_transId = Just "2149186775",
--         transactionResponse_refTransID = Just "",
--         transactionResponse_transHash = Just "C85B15CED28462974F1114DB07A16C39",
--         transactionResponse_testRequest = Just "0",
--         transactionResponse_accountNumber = Just "XXXX0015",
--         transactionResponse_accountType = Just "MasterCard",
--         transactionResponse_messages = Just $ ArrayOf $ pure $ TransactionResponse_message (Just "1") (Just "This transaction has been approved."),
--         transactionResponse_userFields = Just $ ArrayOf [
--             UserField "MerchantDefinedFieldName1" "MerchantDefinedFieldValue1",
--             UserField "favorite_color" "blue"
--         ]
--       }
--       response = CreateTransactionResponse (Just "123456") testMessages Nothing transactionResponse Nothing
--   in assertEncodes text response

-- test_getHostedProfilePageResponse :: Assertion
-- test_getHostedProfilePageResponse =
--   let text = [r|
-- {
--     "token": "e3X1JmlCM01EV4HVLqJhdbfStNUmKMkeQ/bm+jBGrFwpeLnaX3E6wmquJZtLXEyMHlcjhNPx471VoGzyrYF1/VIDKk/qcDKT9BShN64Noft0toiYq07nn1CD+w4AzK2kwpSJkjS3I92h9YompnDXSkPKJWopwUesi6n/trJ96CP/m4rf4Xv6vVQqS0DEu+e+foNGkobJwjop2qHPYOp6e+oNGNIYcGYc06VkwE3kQ+ZbBpBhlkKRYdjJdBYRwdSRtcE7YPia2ENTFGNuMYZvFv7rBaoBftWMvapK7Leb1QcE1uQ+t/9X0wlamazbJmubdiE4Gg5GSiFFeVMcMEhUGJyloDCkTzY/Yv1tg0kAK7GfLXLcD+1pwu+YAR4MasCwnFMduwOc3sFOEWmhnU/cvQ==",
--     "messages": {
--         "resultCode": "Ok",
--         "message": [
--             {
--                 "code": "I00001",
--                 "text": "Successful."
--             }
--         ]
--     }
-- }
-- |]
--       response = GetHostedProfilePageResponse Nothing testMessages Nothing $ Just "e3X1JmlCM01EV4HVLqJhdbfStNUmKMkeQ/bm+jBGrFwpeLnaX3E6wmquJZtLXEyMHlcjhNPx471VoGzyrYF1/VIDKk/qcDKT9BShN64Noft0toiYq07nn1CD+w4AzK2kwpSJkjS3I92h9YompnDXSkPKJWopwUesi6n/trJ96CP/m4rf4Xv6vVQqS0DEu+e+foNGkobJwjop2qHPYOp6e+oNGNIYcGYc06VkwE3kQ+ZbBpBhlkKRYdjJdBYRwdSRtcE7YPia2ENTFGNuMYZvFv7rBaoBftWMvapK7Leb1QcE1uQ+t/9X0wlamazbJmubdiE4Gg5GSiFFeVMcMEhUGJyloDCkTzY/Yv1tg0kAK7GfLXLcD+1pwu+YAR4MasCwnFMduwOc3sFOEWmhnU/cvQ=="
--   in assertEncodes text response

responseTests :: TestTree
responseTests = testGroup "API Responses Encode and Decode to JSON correctly" [
      -- testCase "authenticateTestResponse" $ assertEncodes apiExpected_authenticateTestResponse apiActual_authenticateTestResponse,
      -- testCase "createCustomerProfileResponse" $ assertEncodes apiExpected_createCustomerProfileResponse apiActual_createCustomerProfileResponse,
      -- testCase "getCustomerProfileResponse" $ assertDecodes (undefined :: GetCustomerProfileResponse) apiExpected_getCustomerProfileResponse,
      -- testCase "getCustomerProfileIdsResponse" test_getCustomerProfileIdsResponse,
      -- testCase "updateCustomerProfileResponse" test_updateCustomerProfileResponse,
      -- testCase "deleteCustomerProfileResponse" test_deleteCustomerProfileResponse,
      -- testCase "createCustomerPaymentProfileResponse" test_createCustomerPaymentProfileResponse,
      -- testCase "getCustomerPaymentProfileResponse" test_getCustomerPaymentProfileResponse,
      -- testCase "getCustomerPaymentProfileListResponse" test_getCustomerPaymentProfileListResponse,
      -- testCase "validateCustomerPaymentProfileResponse" test_validateCustomerPaymentProfileResponse,
      -- testCase "updateCustomerPaymentProfileResponse" test_updateCustomerPaymentProfileResponse,
      -- testCase "deleteCustomerPaymentProfileResponse" test_deleteCustomerPaymentProfileResponse,
      -- testCase "createCustomerProfileFromTransactionResponse" test_createCustomerProfileFromTransactionResponse,
      -- testCase "getHostedProfilePageResponse" test_getHostedProfilePageResponse,
      -- testCase "chargeCustomerProfile" test_chargeCustomerProfile
     ]
