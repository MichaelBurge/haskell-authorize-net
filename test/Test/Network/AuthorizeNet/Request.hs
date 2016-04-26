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


test_createCustomerPaymentProfileRequest :: Assertion
test_createCustomerPaymentProfileRequest =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<createCustomerPaymentProfileRequest xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <merchantAuthentication>
    <name>API_LOGIN_ID</name>
    <transactionKey>API_TRANSACTION_KEY</transactionKey>
  </merchantAuthentication>
  <customerProfileId>10000</customerProfileId>
  <paymentProfile>
    <billTo>
      <firstName>John</firstName>
      <lastName>Doe</lastName>
      <company></company>
      <address>123 Main St.</address>
      <city>Bellevue</city>
      <state>WA</state>
      <zip>98004</zip>
      <country>USA</country>
      <phoneNumber>000-000-0000</phoneNumber>
      <faxNumber></faxNumber>
    </billTo>
    <payment>
      <creditCard>
        <cardNumber>4111111111111111</cardNumber>
        <expirationDate>2023-12</expirationDate>
      </creditCard>
    </payment>
  </paymentProfile>
  <validationMode>liveMode</validationMode>
</createCustomerPaymentProfileRequest>
|]
      actual = CreateCustomerPaymentProfileRequest testMerchantAuthentication 10000 (CustomerPaymentProfile {
  customerPaymentProfile_customerType = Nothing,
  customerPaymentProfile_billTo = Just CustomerAddress {
      customerAddress_firstName = Just "John",
      customerAddress_lastName = Just "Doe",
      customerAddress_company = Just "",
      customerAddress_address = Just "123 Main St.",
      customerAddress_city = Just "Bellevue",
      customerAddress_state = Just "WA",
      customerAddress_zip = Just "98004",
      customerAddress_country = Just "USA",
      customerAddress_phoneNumber = Just "000-000-0000",
      customerAddress_faxNumber = Just "",
      customerAddress_email = Nothing
      },
  customerPaymentProfile_payment = Just $ Payment_creditCard $ CreditCard {
      creditCard_cardNumber = "4111111111111111",
      creditCard_expirationDate = "2023-12",
      creditCard_cardCode = Nothing,
      creditCard_isPaymentToken = Nothing,
      creditCard_cryptogram = Nothing
      },
  customerPaymentProfile_driversLicense = Nothing,
  customerPaymentProfile_taxId = Nothing
  }) Validation_liveMode
  in assertEncodes expected actual

test_getCustomerPaymentProfileRequest :: Assertion
test_getCustomerPaymentProfileRequest =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<getCustomerPaymentProfileRequest xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <merchantAuthentication>
    <name>API_LOGIN_ID</name>
    <transactionKey>API_TRANSACTION_KEY</transactionKey>
  </merchantAuthentication>
  <customerProfileId>10000</customerProfileId>
  <customerPaymentProfileId>20000</customerPaymentProfileId>
</getCustomerPaymentProfileRequest>
|]
      actual = GetCustomerPaymentProfileRequest testMerchantAuthentication 10000 20000
  in assertEncodes expected actual

 
test_getCustomerPaymentProfileListRequest :: Assertion
test_getCustomerPaymentProfileListRequest =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<getCustomerPaymentProfileListRequest xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <merchantAuthentication>
    <name>API_LOGIN_ID</name>
    <transactionKey>API_TRANSACTION_KEY</transactionKey>
  </merchantAuthentication>
  <searchType>cardsExpiringInMonth</searchType>
  <month>2020-12</month>
  <sorting>
        <orderBy>id</orderBy>
    <orderDescending>false</orderDescending>
  </sorting>
  <paging>
    <limit>1000</limit>
    <offset>1</offset>
  </paging>
</getCustomerPaymentProfileListRequest>
|]
      searchType = SearchType_cardsExpiringInMonth
      sorting = CustomerPaymentProfileSorting OrderField_id False
      paging = Paging 1000 1
      actual =  GetCustomerPaymentProfileListRequest testMerchantAuthentication searchType "2020-12" sorting paging
  in assertEncodes expected actual

test_validateCustomerPaymentProfileRequest :: Assertion
test_validateCustomerPaymentProfileRequest =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<validateCustomerPaymentProfileRequest xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <merchantAuthentication>
    <name>API_LOGIN_ID</name>
    <transactionKey>API_TRANSACTION_KEY</transactionKey>
  </merchantAuthentication>
  <customerProfileId>10000</customerProfileId>
  <customerPaymentProfileId>20000</customerPaymentProfileId>
  <validationMode>liveMode</validationMode>
</validateCustomerPaymentProfileRequest>
|]
      actual = ValidateCustomerPaymentProfileRequest testMerchantAuthentication 10000 20000 Nothing Nothing Validation_liveMode
  in assertEncodes expected actual

test_updateCustomerPaymentProfileRequest :: Assertion
test_updateCustomerPaymentProfileRequest =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<updateCustomerPaymentProfileRequest xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <merchantAuthentication>
    <name>API_LOGIN_ID</name>
    <transactionKey>API_TRANSACTION_KEY</transactionKey>
  </merchantAuthentication>
  <customerProfileId>10000</customerProfileId>
  <paymentProfile>
    <billTo>
      <firstName>John</firstName>
      <lastName>Doe</lastName>
      <company></company>
      <address>123 Main St.</address>
      <city>Bellevue</city>
      <state>WA</state>
      <zip>98004</zip>
      <country>USA</country>
      <phoneNumber>000-000-0000</phoneNumber>
      <faxNumber></faxNumber>
    </billTo>
    <payment>
      <creditCard>
        <cardNumber>4111111111111111</cardNumber>
        <expirationDate>2026-01</expirationDate>
      </creditCard>
    </payment>
    <customerPaymentProfileId>20000</customerPaymentProfileId>
  </paymentProfile>
  <validationMode>liveMode</validationMode>
</updateCustomerPaymentProfileRequest>
|]
      paymentProfile = CustomerPaymentProfileEx {
        customerPaymentProfileEx_customerType             = Nothing,
        customerPaymentProfileEx_billTo                   = Just CustomerAddress {
            customerAddress_firstName = Just "John",
            customerAddress_lastName = Just "Doe",
            customerAddress_company = Just "",
            customerAddress_address = Just "123 Main St.",
            customerAddress_city = Just "Bellevue",
            customerAddress_state = Just "WA",
            customerAddress_zip = Just "98004",
            customerAddress_country = Just "USA",
            customerAddress_phoneNumber = Just "000-000-0000",
            customerAddress_faxNumber = Just "",
            customerAddress_email = Nothing
            },
        customerPaymentProfileEx_payment                  = Just $ Payment_creditCard $ CreditCard {
            creditCard_cardNumber = "4111111111111111",
            creditCard_expirationDate = "2026-01",
            creditCard_cardCode = Nothing,
            creditCard_isPaymentToken = Nothing,
            creditCard_cryptogram = Nothing
            },
        customerPaymentProfileEx_driversLicense           = Nothing,
        customerPaymentProfileEx_taxId                    = Nothing,
        customerPaymentProfileEx_customerPaymentProfileId = Just 20000
        }
      actual = UpdateCustomerPaymentProfileRequest testMerchantAuthentication 10000 paymentProfile Validation_liveMode
  in assertEncodes expected actual

test_deleteCustomerPaymentProfileRequest :: Assertion
test_deleteCustomerPaymentProfileRequest =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<deleteCustomerPaymentProfileRequest xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <merchantAuthentication>
    <name>API_LOGIN_ID</name>
    <transactionKey>API_TRANSACTION_KEY</transactionKey>
  </merchantAuthentication>
  <customerProfileId>10000</customerProfileId>
  <customerPaymentProfileId>20000</customerPaymentProfileId>
</deleteCustomerPaymentProfileRequest>
|]
      actual = DeleteCustomerPaymentProfileRequest testMerchantAuthentication 10000 20000
  in assertEncodes expected actual

test_getHostedProfilePageRequest :: Assertion
test_getHostedProfilePageRequest =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<getHostedProfilePageRequest xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <merchantAuthentication>
    <name>API_LOGIN_ID</name>
    <transactionKey>API_TRANSACTION_KEY</transactionKey>
  </merchantAuthentication>
  <customerProfileId>123456</customerProfileId>
  <hostedProfileSettings>
    <setting>
      <settingName>hostedProfileReturnUrl</settingName>
      <settingValue>https://returnurl.com/return/</settingValue>
    </setting>
    <setting>
      <settingName>hostedProfileReturnUrlText</settingName>
      <settingValue>Continue to confirmation page.</settingValue>
    </setting>
          <setting>
                <settingName>hostedProfilePageBorderVisible</settingName>
                 <settingValue>true</settingValue>
          </setting>
 </hostedProfileSettings>
</getHostedProfilePageRequest>
|]
      actual = GetHostedProfilePageRequest testMerchantAuthentication Nothing 123456 $ Just $ ArrayOfSetting $ ArrayOf [
        Setting SettingName_hostedProfileReturnUrl "https://returnurl.com/return/",
        Setting SettingName_hostedProfileReturnUrlText "Continue to confirmation page.",
        Setting SettingName_hostedProfilePageBorderVisible "true"
        ]
  in assertEncodes expected actual


test_createCustomerProfileFromTransactionRequest :: Assertion
test_createCustomerProfileFromTransactionRequest =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<createCustomerProfileFromTransactionRequest xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
        <merchantAuthentication>
                <name>API_LOGIN_ID</name>
                <transactionKey>API_TRANSACTION_KEY</transactionKey>
        </merchantAuthentication>
        <transId>122</transId>
</createCustomerProfileFromTransactionRequest>                   
|]
      actual = CreateCustomerProfileFromTransactionRequest testMerchantAuthentication 122 Nothing Nothing
  in assertEncodes expected actual

test_chargeCustomerProfileRequest :: Assertion
test_chargeCustomerProfileRequest =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<createTransactionRequest xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <merchantAuthentication>
    <name>API_LOGIN_ID</name>
    <transactionKey>API_TRANSACTION_KEY</transactionKey>
  </merchantAuthentication>
  <refId>123456</refId>
  <transactionRequest>
    <transactionType>authCaptureTransaction</transactionType>
    <amount>5</amount>
    <profile>
      <customerProfileId>27388924</customerProfileId>
      <paymentProfile>
        <paymentProfileId>25000332</paymentProfileId>
      </paymentProfile>
    </profile>
    <order>
      <invoiceNumber>INV-12345</invoiceNumber>
      <description>Product Description</description>
    </order>
    <lineItems>
      <lineItem>
        <itemId>1</itemId>
        <name>vase</name>
        <description>Cannes logo </description>
        <quantity>18</quantity>
        <unitPrice>45.00</unitPrice>
      </lineItem>
    </lineItems>
    <poNumber>456654</poNumber>
    <shipTo>
      <firstName>China</firstName>
      <lastName>Bayles</lastName>
      <company>Thyme for Tea</company>
      <address>12 Main Street</address>
      <city>Pecan Springs</city>
      <state>TX</state>
      <zip>44628</zip>
      <country>USA</country>
    </shipTo>
    <customerIP>192.168.1.1</customerIP>
  </transactionRequest>
</createTransactionRequest>                   
|]
      transactionRequest = (mkTransactionRequest Transaction_authCaptureTransaction "5") {
        transactionRequest_profile = Just $ mkCustomerProfilePayment {
            customerProfilePayment_customerProfileId = Just 27388924,
            customerProfilePayment_paymentProfile = Just $ PaymentProfile 25000332 Nothing
            },
        transactionRequest_order = Just $ Order (Just "INV-12345") (Just "Product Description"),
        transactionRequest_lineItems = Just $ LineItems $ ArrayOf $ [ LineItem {
            lineItem_itemId = "1",
            lineItem_name = "vase",
            lineItem_description = Just "Cannes logo ",
            lineItem_quantity = "18",
            lineItem_unitPrice = "45.00",
            lineItem_taxable = Nothing
            } ],
        transactionRequest_poNumber = Just "456654",
        transactionRequest_shipTo = Just CustomerAddress {
            customerAddress_firstName = Just "China",
            customerAddress_lastName = Just "Bayles",
            customerAddress_company = Just "Thyme for Tea",
            customerAddress_address = Just "12 Main Street",
            customerAddress_city = Just "Pecan Springs",
            customerAddress_state = Just "TX",
            customerAddress_zip = Just "44628",
            customerAddress_country = Just "USA",
            customerAddress_phoneNumber = Nothing,
            customerAddress_faxNumber = Nothing,
            customerAddress_email = Nothing
            },
          transactionRequest_customerIP = Just "192.168.1.1"
        }
      actual = CreateTransactionRequest testMerchantAuthentication (Just "123456") transactionRequest
  in assertEncodes expected actual

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
--       request = 
--   in assertEncodes text request

requestTests :: TestTree
requestTests = testGroup "API Requests Encode and Decode to JSON correctly" [
  testCase "authenticateTestRequest" test_authenticateTestRequest,
  testCase "createCustomerProfileRequest" test_createCustomerProfileRequest,
  testCase "getCustomerProfileRequest" test_getCustomerProfileRequest,
  testCase "getCustomerProfileIdsRequest" test_getCustomerProfileIdsRequest, 
  testCase "updateCustomerProfileRequest" test_updateCustomerProfileRequest,
  testCase "deleteCustomerProfileRequest" test_deleteCustomerProfileRequest,
  testCase "createCustomerPaymentProfileRequest" test_createCustomerPaymentProfileRequest,
  testCase "getCustomerPaymentProfileRequest" test_getCustomerPaymentProfileRequest,
  testCase "validateCustomerPaymentProfileRequest" test_validateCustomerPaymentProfileRequest,
  testCase "updateCustomerPaymentProfile" test_updateCustomerPaymentProfileRequest,
  testCase "deleteCustomerPaymentProfile" test_deleteCustomerPaymentProfileRequest,
  testCase "getHostedProfilePageRequest" test_getHostedProfilePageRequest,
  testCase "createCustomerProfileFromTransaction" test_createCustomerProfileFromTransactionRequest,
  testCase "chargeCustomerProfile" test_chargeCustomerProfileRequest
  ]
