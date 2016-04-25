{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Test.Network.AuthorizeNet.Request (requestTests) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Data.Monoid
import Text.RawString.QQ

import Test.Tasty
import Test.Tasty.HUnit

import Network.AuthorizeNet.Request
import Network.AuthorizeNet.Types
import Test.Network.AuthorizeNet.Util

-- | These unit tests were created from the examples at http://developer.authorize.net/api/reference/index.html

test_authenticateTestRequest :: Assertion
test_authenticateTestRequest =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>                   
<authenticateTestRequest xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <merchantAuthentication>
        <name>API_LOGIN_ID</name>
        <transactionKey>API_TRANSACTION_KEY</transactionKey>
    </merchantAuthentication>
</authenticateTestRequest>
|]
      actual = AuthenticateTestRequest testMerchantAuthentication
  in assertEncodes expected actual

test_createCustomerProfileRequest :: Assertion
test_createCustomerProfileRequest =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<createCustomerProfileRequest xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">  
   <merchantAuthentication>
     <name>API_LOGIN_ID</name>
     <transactionKey>API_TRANSACTION_KEY</transactionKey>
    </merchantAuthentication>
   <profile>
     <merchantCustomerId>Merchant_Customer_ID</merchantCustomerId>
     <description>Profile description here</description>
     <email>customer-profile-email@here.com</email>
     <paymentProfiles>
       <customerType>individual</customerType>
          <payment>
             <creditCard>
                <cardNumber>4111111111111111</cardNumber>
                <expirationDate>2020-12</expirationDate>
              </creditCard>
           </payment>
      </paymentProfiles>
    </profile>
    <validationMode>testMode</validationMode>
  </createCustomerProfileRequest>
|]
      actual = CreateCustomerProfileRequest testMerchantAuthentication testCustomerProfile Validation_testMode
  in assertEncodes expected actual

test_getCustomerProfileRequest :: Assertion
test_getCustomerProfileRequest =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<getCustomerProfileRequest xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <merchantAuthentication>
    <name>API_LOGIN_ID</name>
    <transactionKey>API_TRANSACTION_KEY</transactionKey>
  </merchantAuthentication>
  <customerProfileId>10000</customerProfileId>
</getCustomerProfileRequest>
|]
      actual = GetCustomerProfileRequest testMerchantAuthentication 10000
  in assertEncodes expected actual

test_getCustomerProfileIdsRequest :: Assertion
test_getCustomerProfileIdsRequest =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<getCustomerProfileIdsRequest xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <merchantAuthentication>
    <name>API_LOGIN_ID</name>
    <transactionKey>API_TRANSACTION_KEY</transactionKey>
  </merchantAuthentication>
</getCustomerProfileIdsRequest>
|]
      actual = GetCustomerProfileIdsRequest testMerchantAuthentication
  in assertEncodes expected actual

test_updateCustomerProfileRequest :: Assertion
test_updateCustomerProfileRequest =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<updateCustomerProfileRequest xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <merchantAuthentication>
    <name>API_LOGIN_ID</name>
    <transactionKey>API_TRANSACTION_KEY</transactionKey>
  </merchantAuthentication>
  <profile>
    <merchantCustomerId>custId123</merchantCustomerId>
    <description>some description</description>
    <email>newaddress@example.com</email>
    <customerProfileId>10000</customerProfileId>
  </profile>
</updateCustomerProfileRequest>
|]
      actual = UpdateCustomerProfileRequest testMerchantAuthentication $ CustomerProfileEx {
  customerProfileEx_customerProfileId = Just 10000,
  customerProfileEx_merchantCustomerId = "custId123",
  customerProfileEx_description = "some description",
  customerProfileEx_email = "newaddress@example.com"
  }
  in assertEncodes expected actual

test_deleteCustomerProfileRequest :: Assertion
test_deleteCustomerProfileRequest =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<deleteCustomerProfileRequest xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <merchantAuthentication>
    <name>API_LOGIN_ID</name>
    <transactionKey>API_TRANSACTION_KEY</transactionKey>
  </merchantAuthentication>
  <customerProfileId>10000</customerProfileId>
</deleteCustomerProfileRequest>
|]
      actual = DeleteCustomerProfileRequest testMerchantAuthentication 10000
  in assertEncodes expected actual


-- apiExpected_createCustomerPaymentProfileRequest :: String
-- apiExpected_createCustomerPaymentProfileRequest = [r|
-- {
--     "createCustomerPaymentProfileRequest": {
--         "merchantAuthentication": {
--             "name": "API_LOGIN_ID",
--             "transactionKey": "API_TRANSACTION_KEY"
--         },
--         "customerProfileId": "10000",
--         "paymentProfile": {
--             "billTo": {
--                 "firstName": "John",
--                 "lastName": "Doe",
--                 "company": "",
--                 "address": "123 Main St.",
--                 "city": "Bellevue",
--                 "state": "WA",
--                 "zip": "98004",
--                 "country": "USA",
--                 "phoneNumber": "000-000-0000",
--                 "faxNumber": ""
--             },
--             "payment": {
--                 "creditCard": {
--                     "cardNumber": "4111111111111111",
--                     "expirationDate": "2023-12"
--                 }
--             }
--         },
--         "validationMode": "liveMode"
--     }
-- }
-- |]

-- apiActual_createCustomerPaymentProfileRequest :: ApiRequest
-- apiActual_createCustomerPaymentProfileRequest = CreateCustomerPaymentProfile testMerchantAuthentication 10000 $ CustomerPaymentProfile {
--   customerPaymentProfile_customerType = Nothing,
--   customerPaymentProfile_billTo = Just CustomerAddress {
--       customerAddress_firstName = Just "John",
--       customerAddress_lastName = Just "Doe",
--       customerAddress_company = Just "",
--       customerAddress_address = Just "123 Main St.",
--       customerAddress_city = Just "Bellevue",
--       customerAddress_state = Just "WA",
--       customerAddress_zip = Just "98004",
--       customerAddress_country = Just "USA",
--       customerAddress_phoneNumber = Just "000-000-0000",
--       customerAddress_faxNumber = Just "",
--       customerAddress_email = Nothing
--       },
--   customerPaymentProfile_payment = Just $ Payment_creditCard $ CreditCard {
--       creditCard_cardNumber = "4111111111111111",
--       creditCard_expirationDate = "2023-12",
--       creditCard_cardCode = Nothing,
--       creditCard_isPaymentToken = Nothing,
--       creditCard_cryptogram = Nothing
--       },
--   customerPaymentProfile_driversLicense = Nothing,
--   customerPaymentProfile_taxId = Nothing
--   }

-- apiExpected_getCustomerPaymentProfileRequest :: String
-- apiExpected_getCustomerPaymentProfileRequest = [r|
-- {
--     "getCustomerPaymentProfileRequest": {
--         "merchantAuthentication": {
--             "name": "API_LOGIN_ID",
--             "transactionKey": "API_TRANSACTION_KEY"
--         },
--         "customerProfileId": "10000",
--         "customerPaymentProfileId": "20000"
--     }
-- }
-- |]

-- apiActual_getCustomerPaymentProfileRequest :: ApiRequest
-- apiActual_getCustomerPaymentProfileRequest = GetCustomerPaymentProfile testMerchantAuthentication 10000 20000

-- apiExpected_getCustomerPaymentProfileListRequest :: String
-- apiExpected_getCustomerPaymentProfileListRequest = [r|
-- { 
--          "getCustomerPaymentProfileListRequest": { 
--                  "merchantAuthentication": { 
--                          "name": "API_LOGIN_ID", 
--                          "transactionKey": "API_TRANSACTION_KEY" 
--                  }, 
--                  "searchType": "cardsExpiringInMonth", 
--                  "month": "2020-12", 
--                  "sorting": { 
--                          "orderBy": "id", 
--                          "orderDescending": "false" 
--                  }, 
--                  "paging": { 
--                          "limit": "1000", 
--                          "offset": "1" 
--                  } 
--          } 
--  }
-- |]

-- apiActual_getCustomerPaymentProfileListRequest :: ApiRequest
-- apiActual_getCustomerPaymentProfileListRequest =
--   let searchType = SearchType_cardsExpiringInMonth
--       sorting = CustomerPaymentProfileSorting OrderField_id False
--       paging = Paging 1000 1
--   in GetCustomerPaymentProfileList testMerchantAuthentication searchType "2020-12" sorting paging

-- apiExpected_validateCustomerPaymentProfileRequest :: String
-- apiExpected_validateCustomerPaymentProfileRequest = [r|
-- {
--     "validateCustomerPaymentProfileRequest": {
--         "merchantAuthentication": {
--             "name": "API_LOGIN_ID",
--             "transactionKey": "API_TRANSACTION_KEY"
--         },
--         "customerProfileId": "10000",
--         "customerPaymentProfileId": "20000",
--         "validationMode": "liveMode"
--     }
-- }
-- |]

-- apiActual_validateCustomerPaymentProfileRequest :: ApiRequest
-- apiActual_validateCustomerPaymentProfileRequest = ValidateCustomerPaymentProfile testMerchantAuthentication 10000 20000 Nothing Nothing Validation_liveMode

-- apiExpected_updateCustomerPaymentProfileRequest :: String
-- apiExpected_updateCustomerPaymentProfileRequest = [r|
-- {
--     "updateCustomerPaymentProfileRequest": {
--         "merchantAuthentication": {
--             "name": "API_LOGIN_ID",
--             "transactionKey": "API_TRANSACTION_KEY"
--         },
--         "customerProfileId": "10000",
--         "paymentProfile": {
--             "billTo": {
--                 "firstName": "John",
--                 "lastName": "Doe",
--                 "company": "",
--                 "address": "123 Main St.",
--                 "city": "Bellevue",
--                 "state": "WA",
--                 "zip": "98004",
--                 "country": "USA",
--                 "phoneNumber": "000-000-0000",
--                 "faxNumber": ""
--             },
--             "payment": {
--                 "creditCard": {
--                     "cardNumber": "4111111111111111",
--                     "expirationDate": "2026-01"
--                 }
--             },
--             "customerPaymentProfileId": "20000"
--         },
--         "validationMode": "liveMode"
--     }
-- }
-- |]

-- apiActual_updateCustomerPaymentProfileRequest :: ApiRequest
-- apiActual_updateCustomerPaymentProfileRequest =
--   let paymentProfile = CustomerPaymentProfileEx {
--         customerPaymentProfileEx_customerType             = Nothing,
--         customerPaymentProfileEx_billTo                   = Just CustomerAddress {
--             customerAddress_firstName = Just "John",
--             customerAddress_lastName = Just "Doe",
--             customerAddress_company = Just "",
--             customerAddress_address = Just "123 Main St.",
--             customerAddress_city = Just "Bellevue",
--             customerAddress_state = Just "WA",
--             customerAddress_zip = Just "98004",
--             customerAddress_country = Just "USA",
--             customerAddress_phoneNumber = Just "000-000-0000",
--             customerAddress_faxNumber = Just "",
--             customerAddress_email = Nothing
--             },
--         customerPaymentProfileEx_payment                  = Just $ Payment_creditCard $ CreditCard {
--             creditCard_cardNumber = "4111111111111111",
--             creditCard_expirationDate = "2026-01",
--             creditCard_cardCode = Nothing,
--             creditCard_isPaymentToken = Nothing,
--             creditCard_cryptogram = Nothing
--             },
--         customerPaymentProfileEx_driversLicense           = Nothing,
--         customerPaymentProfileEx_taxId                    = Nothing,
--         customerPaymentProfileEx_customerPaymentProfileId = Just 20000
--         }
--   in UpdateCustomerPaymentProfile testMerchantAuthentication 10000 paymentProfile Validation_liveMode

-- apiExpected_deleteCustomerPaymentProfileRequest :: String
-- apiExpected_deleteCustomerPaymentProfileRequest = [r|
-- {
--     "deleteCustomerPaymentProfileRequest": {
--         "merchantAuthentication": {
--             "name": "API_LOGIN_ID",
--             "transactionKey": "API_TRANSACTION_KEY"
--         },
--         "customerProfileId": "10000",
--         "customerPaymentProfileId": "20000"
--     }
-- }
-- |]

-- apiActual_deleteCustomerPaymentProfileRequest :: ApiRequest
-- apiActual_deleteCustomerPaymentProfileRequest = DeleteCustomerPaymentProfile testMerchantAuthentication 10000 20000

-- apiExpected_createCustomerProfileFromTransactionRequest :: String
-- apiExpected_createCustomerProfileFromTransactionRequest = [r|
-- {
--     "createCustomerProfileFromTransactionRequest": {
--         "merchantAuthentication": {
--             "name": "API_LOGIN_ID",
--             "transactionKey": "API_TRANSACTION_KEY"
--         },
--         "transId": "122"
--     }
-- }
-- |]

-- apiActual_createCustomerProfileFromTransactionRequest :: ApiRequest
-- apiActual_createCustomerProfileFromTransactionRequest = CreateCustomerProfileFromTransaction testMerchantAuthentication 122 Nothing Nothing

-- apiExpected_chargeCustomerProfileRequest :: String
-- apiExpected_chargeCustomerProfileRequest = [r|
-- {
--     "createTransactionRequest": {
--         "merchantAuthentication": {
--             "name": "API_LOGIN_ID",
--             "transactionKey": "API_TRANSACTION_KEY"
--         },
--         "refId": "123456",
--         "transactionRequest": {
--             "transactionType": "authCaptureTransaction",
--             "amount": "45",
--               "profile": {
--                         "customerProfileId": "27388924",
--                         "paymentProfile": { "paymentProfileId": "25000332" }
--                         },
--             "lineItems": {
--                 "lineItem": {
--                     "itemId": "1",
--                     "name": "vase",
--                     "description": "Cannes logo",
--                     "quantity": "18",
--                     "unitPrice": "45.00"
--                 }
--             }
--         }
--     }
-- }
-- |]

-- apiActual_chargeCustomerProfileRequest :: ApiRequest
-- apiActual_chargeCustomerProfileRequest =
--   let transactionRequest = (mkTransactionRequest Transaction_authCaptureTransaction "45") {
--         transactionRequest_profile = Just $ mkCustomerProfilePayment {
--             customerProfilePayment_customerProfileId = Just 27388924,
--             customerProfilePayment_paymentProfile = Just $ PaymentProfile 25000332 Nothing
--             },
--         transactionRequest_lineItems = Just $ LineItems $ ArrayOf $ [ LineItem {
--             lineItem_itemId = "1",
--             lineItem_name = "vase",
--             lineItem_description = Just "Cannes logo",
--             lineItem_quantity = "18",
--             lineItem_unitPrice = "45.00",
--             lineItem_taxable = Nothing
--             } ]
--         }
--   in CreateTransaction testMerchantAuthentication (Just "123456") transactionRequest

-- test_getHostedProfilePageRequest =
--   let text = [r|
-- {
--     "getHostedProfilePageRequest": {
--         "merchantAuthentication": {
--             "name": "API_LOGIN_ID",
--             "transactionKey": "API_TRANSACTION_KEY"
--         },
--         "customerProfileId": "123456",
--         "hostedProfileSettings": {
--             "setting": [
--                 {
--                     "settingName": "hostedProfileReturnUrl",
--                     "settingValue": "https://returnurl.com/return/"
--                 },
--                 {
--                     "settingName": "hostedProfileReturnUrlText",
--                     "settingValue": "Continue to confirmation page."
--                 },
--                 {
--                     "settingName": "hostedProfilePageBorderVisible",
--                     "settingValue": "true"
--                 }
--             ]
--         }
--     }
-- }               
-- |]
--       request = GetHostedProfilePage testMerchantAuthentication Nothing 123456 $ Just $ ArrayOfSetting $ ArrayOf [
--         Setting SettingName_hostedProfileReturnUrl "https://returnurl.com/return/",
--         Setting SettingName_hostedProfileReturnUrlText "Continue to confirmation page.",
--         Setting SettingName_hostedProfilePageBorderVisible "true"
--         ]
--   in assertEncodes text request

requestTests :: TestTree
requestTests = testGroup "API Requests Encode and Decode to JSON correctly" [
  testCase "authenticateTestRequest" test_authenticateTestRequest,
  testCase "createCustomerProfileRequest" test_createCustomerProfileRequest,
  testCase "getCustomerProfileRequest" test_getCustomerProfileRequest,
  testCase "getCustomerProfileIdsRequest" test_getCustomerProfileIdsRequest, 
  testCase "updateCustomerProfileRequest" test_updateCustomerProfileRequest,
  testCase "deleteCustomerProfileRequest" test_deleteCustomerProfileRequest
  -- testCase "createCustomerPaymentProfileRequest" $ assertEncodes apiExpected_createCustomerPaymentProfileRequest apiActual_createCustomerPaymentProfileRequest,
  -- testCase "getCustomerPaymentProfileRequest" $ assertEncodes apiExpected_getCustomerPaymentProfileRequest apiActual_getCustomerPaymentProfileRequest,
  -- testCase "validateCustomerPaymentProfileRequest" $ assertEncodes apiExpected_validateCustomerPaymentProfileRequest apiActual_validateCustomerPaymentProfileRequest,
  -- testCase "updateCustomerPaymentProfile" $ assertEncodes apiExpected_updateCustomerPaymentProfileRequest apiActual_updateCustomerPaymentProfileRequest,
  -- testCase "deleteCustomerPaymentProfile" $ assertEncodes apiExpected_deleteCustomerPaymentProfileRequest apiActual_deleteCustomerPaymentProfileRequest,
  -- testCase "createCustomerProfileFromTransaction" $ assertEncodes apiExpected_createCustomerProfileFromTransactionRequest apiActual_createCustomerProfileFromTransactionRequest,
  -- testCase "getHostedProfilePageRequest" test_getHostedProfilePageRequest,
  -- testCase "chargeCustomerProfile" $ assertEncodes apiExpected_chargeCustomerProfileRequest apiActual_chargeCustomerProfileRequest
  ]
