{-# LANGUAGE QuasiQuotes, OverloadedStrings, OverloadedLists #-}

module Test.Network.AuthorizeNet.Response (responseTests) where

import Network.AuthorizeNet.Response
import Network.AuthorizeNet.Types
import Network.AuthorizeNet.Util

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

test_authenticateTestResponse :: Assertion
test_authenticateTestResponse =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<authenticateTestResponse xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
    <messages>
        <resultCode>Ok</resultCode>
        <message>
            <code>I00001</code>
            <text>Successful.</text>
        </message>
    </messages>
</authenticateTestResponse>
|]
      actual = mkAuthenticateTestResponse testMessages
  in assertEncodes expected actual

test_createCustomerProfileResponse :: Assertion
test_createCustomerProfileResponse =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<createCustomerProfileResponse xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <messages>
    <resultCode>Ok</resultCode>
    <message>
      <code>I00001</code>
      <text>Successful.</text>
    </message>
  </messages>
  <customerProfileId>10000</customerProfileId>
<customerPaymentProfileIdList>
   <numericString>20000</numericString>
   <numericString>20001</numericString>
</customerPaymentProfileIdList>
<customerShippingAddressIdList>
   <numericString>30000</numericString>
   <numericString>30001</numericString>
</customerShippingAddressIdList>
<validationDirectResponseList>
<string>1,1,1,This transaction has been approved.,000000,Y,2000000000,none,Test transaction for ValidateCustomerPaymentProfile.,0.01,CC,auth_only,custId123,John,Doe,,123 Main St.,Bellevue,WA,98004,USA,000-000-0000,,mark@example.com,,,,,,,,,0.00,0.00,0.00,,none,D18EB6B211FE0BBF556B271FDA6F92EE,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,</string>
  </validationDirectResponseList>  
</createCustomerProfileResponse>                   
|]
      actual = CreateCustomerProfileResponse {
  createCustomerProfileResponse_refId = Nothing,                         
  createCustomerProfileResponse_messages = testMessages,              
  createCustomerProfileResponse_sessionToken = Nothing,      
  createCustomerProfileResponse_customerProfileId = Just 10000,
  createCustomerProfileResponse_customerPaymentProfileIdList = [20000,20001],
  createCustomerProfileResponse_customerShippingAddressIdList = [30000,30001],
  createCustomerProfileResponse_validationDirectResponseList = ArrayOfString ["1,1,1,This transaction has been approved.,000000,Y,2000000000,none,Test transaction for ValidateCustomerPaymentProfile.,0.01,CC,auth_only,custId123,John,Doe,,123 Main St.,Bellevue,WA,98004,USA,000-000-0000,,mark@example.com,,,,,,,,,0.00,0.00,0.00,,none,D18EB6B211FE0BBF556B271FDA6F92EE,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,"]
  }
  in assertEncodes expected actual

test_getCustomerProfileResponse :: Assertion
test_getCustomerProfileResponse =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<getCustomerProfileResponse xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
   <messages>
      <resultCode>Ok</resultCode>
      <message>
         <code>I00001</code>
         <text>Successful.</text>
      </message>
   </messages>
   <profile>
      <merchantCustomerId>CUST001</merchantCustomerId>
      <description>Profile created by Subscription: 3078153</description>
      <email>joe@mail.com</email>
      <customerProfileId>39598611</customerProfileId>
      <paymentProfiles>
         <customerType>individual</customerType>
         <billTo>
            <firstName>John</firstName>
            <lastName>Smith</lastName>
         </billTo>
         <customerPaymentProfileId>35936989</customerPaymentProfileId>
         <payment>
           <creditCard>
               <cardNumber>XXXX1111</cardNumber>
               <expirationDate>XXXX</expirationDate>
            </creditCard>
         </payment>
      </paymentProfiles>
   </profile>
   <subscriptionIds>
      <subscriptionId>3078153</subscriptionId>
      <subscriptionId>3078154</subscriptionId>
   </subscriptionIds>
</getCustomerProfileResponse>
|]
      profile = CustomerProfileMasked {
        customerProfileMasked_merchantCustomerId = Just "CUST001",
        customerProfileMasked_description = Just "Profile created by Subscription: 3078153",
        customerProfileMasked_email = Just "joe@mail.com",
        customerProfileMasked_customerProfileId = Just 39598611,
        customerProfileMasked_paymentProfiles = [
          (mkCustomerPaymentProfileMasked 35936989) {
            customerPaymentProfileMasked_customerType = Just CustomerType_individual,
            customerPaymentProfileMasked_billTo = Just $ mkCustomerAddress {
              customerAddress_firstName = Just "John",
              customerAddress_lastName = Just "Smith"
            },
            customerPaymentProfileMasked_payment = Just $ PaymentMasked_creditCard $ mkCreditCardMasked "XXXX1111" "XXXX"
          }
        ],
        customerProfileMasked_shipToList = []
      }
      subscriptionIds = Just $ SubscriptionIdList $ ArrayOf [ 3078153, 3078154 ]
      actual = GetCustomerProfileResponse Nothing testMessages Nothing profile subscriptionIds
  in assertEncodes expected actual

test_getCustomerProfileResponse2 :: Assertion
test_getCustomerProfileResponse2 =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?><getCustomerProfileResponse xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd"><messages><resultCode>Ok</resultCode><message><code>I00001</code><text>Successful.</text></message></messages><profile><merchantCustomerId>1</merchantCustomerId><description>MichaelBurge</description><email>michaelburge@pobox.com</email><customerProfileId>40243901</customerProfileId></profile></getCustomerProfileResponse>
|]
      profile = CustomerProfileMasked {
        customerProfileMasked_merchantCustomerId = Just "1",
        customerProfileMasked_description = Just "MichaelBurge",
        customerProfileMasked_email = Just "michaelburge@pobox.com",
        customerProfileMasked_customerProfileId = Just 40243901,
        customerProfileMasked_paymentProfiles = [],
        customerProfileMasked_shipToList = []
      }
      actual = GetCustomerProfileResponse Nothing testMessages Nothing profile Nothing
  in assertEncodes expected actual

test_getCustomerProfileIdsResponse :: Assertion
test_getCustomerProfileIdsResponse =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<getCustomerProfileIdsResponse xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <messages>
    <resultCode>Ok</resultCode>
    <message>
      <code>I00001</code>
      <text>Successful.</text>
    </message>
  </messages>
  <ids>
    <numericString>10000</numericString>
    <numericString>10001</numericString>
    <numericString>10002</numericString>
  </ids>
</getCustomerProfileIdsResponse>
|]
      actual = GetCustomerProfileIdsResponse Nothing testMessages Nothing $ [10000, 10001, 10002]
  in assertEncodes expected actual

test_updateCustomerProfileResponse :: Assertion
test_updateCustomerProfileResponse =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<updateCustomerProfileResponse xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <messages>
    <resultCode>Ok</resultCode>
    <message>
      <code>I00001</code>
      <text>Successful.</text>
    </message>
  </messages>
</updateCustomerProfileResponse>
|]
      actual = UpdateCustomerProfileResponse Nothing testMessages Nothing
  in assertEncodes expected actual

test_deleteCustomerProfileResponse :: Assertion
test_deleteCustomerProfileResponse =
  let expected = [r|
<?xml version="1.0" encoding="utf-8"?>
<deleteCustomerProfileResponse xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <messages>
    <resultCode>Ok</resultCode>
    <message>
      <code>I00001</code>
      <text>Successful.</text>
    </message>
  </messages>
</deleteCustomerProfileResponse>                   
|]
      actual = DeleteCustomerProfileResponse Nothing testMessages Nothing
  in assertEncodes expected actual

test_createCustomerPaymentProfileResponse :: Assertion
test_createCustomerPaymentProfileResponse =
  let text = [r|
<?xml version="1.0" encoding="utf-8"?>
<createCustomerPaymentProfileResponse xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <messages>
    <resultCode>Ok</resultCode>
    <message>
      <code>I00001</code>
      <text>Successful.</text>
    </message>
  </messages>
  <customerPaymentProfileId>20000</customerPaymentProfileId>
  <validationDirectResponse>1,1,1,This transaction has been approved.,000000,Y,2000000000,none,Test transaction for ValidateCustomerPaymentProfile.,0.01,CC,auth_only,custId123,John,Doe,,123 Main St.,Bellevue,WA,98004,USA,000-000-0000,,mark@example.com,,,,,,,,,0.00,0.00,0.00,,none,D18EB6B211FE0BBF556B271FDA6F92EE,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,</validationDirectResponse>
</createCustomerPaymentProfileResponse>               
|]
      response = CreateCustomerPaymentProfileResponse Nothing testMessages Nothing (Just 20000) $ Just "1,1,1,This transaction has been approved.,000000,Y,2000000000,none,Test transaction for ValidateCustomerPaymentProfile.,0.01,CC,auth_only,custId123,John,Doe,,123 Main St.,Bellevue,WA,98004,USA,000-000-0000,,mark@example.com,,,,,,,,,0.00,0.00,0.00,,none,D18EB6B211FE0BBF556B271FDA6F92EE,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,"
  in assertEncodes text response

test_getCustomerPaymentProfileResponse :: Assertion
test_getCustomerPaymentProfileResponse =
  let text = [r|
<?xmlversion='1.0'encoding='utf-8'?>          
<getCustomerPaymentProfileResponse xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
   <messages>
      <resultCode>Ok</resultCode>
      <message>
         <code>I00001</code>
         <text>Successful.</text>
      </message>
   </messages>
   <paymentProfile>
      <customerType>individual</customerType>
      <billTo>
         <firstName>John</firstName>
         <lastName>Smith</lastName>
      </billTo>
      <customerProfileId>39598611</customerProfileId>
      <customerPaymentProfileId>35936989</customerPaymentProfileId>
      <payment>
         <creditCard>
            <cardNumber>XXXX1111</cardNumber>
            <expirationDate>XXXX</expirationDate>
         </creditCard>
      </payment>
      <subscriptionIds>
         <subscriptionId>3078153</subscriptionId>
         <subscriptionId>3078154</subscriptionId>
      </subscriptionIds>
   </paymentProfile>
</getCustomerPaymentProfileResponse>               
|]
      response = GetCustomerPaymentProfileResponse Nothing testMessages Nothing $ Just $ (mkCustomerPaymentProfileMasked 39598611) {
        customerPaymentProfileMasked_customerType = Just CustomerType_individual,
        customerPaymentProfileMasked_billTo = Just $ mkCustomerAddress {
          customerAddress_firstName = Just "John",
          customerAddress_lastName = Just "Smith"
          },
        customerPaymentProfileMasked_customerProfileId = Just 39598611,
        customerPaymentProfileMasked_customerPaymentProfileId = 35936989,
        customerPaymentProfileMasked_payment = Just $ PaymentMasked_creditCard CreditCardMasked {
          creditCardMasked_cardNumber = "XXXX1111",
          creditCardMasked_expirationDate = "XXXX",
          creditCardMasked_cardType = Nothing,
          creditCardMasked_cardArt = Nothing
        },
        customerPaymentProfileMasked_subscriptionIds = Just [ 3078153, 3078154 ]
      }
  in assertEncodes text response

test_getCustomerPaymentProfileListResponse :: Assertion
test_getCustomerPaymentProfileListResponse =
  let text = [r|
<?xml version="1.0" encoding="utf-8"?> 
<getCustomerPaymentProfileListResponse xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <messages>
    <resultCode>Ok</resultCode>
    <message>
      <code>I00001</code>
      <text>Successful.</text>
    </message>
  </messages>
  <totalNumInResultSet>
    1
  </totalNumInResultSet>
  <paymentProfiles>
    <paymentProfile>
      <customerPaymentProfileId>1051079</customerPaymentProfileId>
      <customerProfileId>918787</customerProfileId>
      <billTo>
        <firstName>John</firstName>
        <lastName>Smith</lastName>
      </billTo>
      <payment>
        <creditCard>
          <cardNumber>XXXX1111</cardNumber>
          <expirationDate>XXXX</expirationDate>
        </creditCard>
      </payment>
    </paymentProfile>
  </paymentProfiles>
</getCustomerPaymentProfileListResponse>               
|]
      response = GetCustomerPaymentProfileListResponse Nothing testMessages Nothing 1 $
                 Just $ ArrayOfCustomerPaymentProfileListItem $ ArrayOf $ pure $
                 CustomerPaymentProfileListItem {
                   customerPaymentProfileListItem_customerPaymentProfileId = 1051079,
                   customerPaymentProfileListItem_customerProfileId        = 918787,
                   customerPaymentProfileListItem_billTo                   = mkCustomerAddress {
                       customerAddress_firstName = Just "John",
                       customerAddress_lastName = Just "Smith"
                   },
                   customerPaymentProfileListItem_payment = PaymentMasked_creditCard $ mkCreditCardMasked "XXXX1111" "XXXX"
               }
  in assertEncodes text response

test_validateCustomerPaymentProfileResponse :: Assertion
test_validateCustomerPaymentProfileResponse =
  let text = [r|
<?xml version="1.0" encoding="utf-8"?>
<validateCustomerPaymentProfileResponse xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <messages>
    <resultCode>Ok</resultCode>
    <message>
      <code>I00001</code>
      <text>Successful.</text>
    </message>
  </messages>
  <directResponse>1,1,1,This transaction has been approved.,000000,Y,2000000003,none,Test transaction for ValidateCustomerPaymentProfile.,0.01,CC,auth_only,custId123,John,Doe,,123 Main St.,Bellevue,WA,98004,USA,000-000-0000,,mark@example.com,John,Doe,,123 Main St.,Bellevue,WA,98004,USA,0.00,0.00,0.00,,none,D18EB6B211FE0BBF556B271FDA6F92EE,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,</directResponse>
</validateCustomerPaymentProfileResponse>               
|]
      response = ValidateCustomerPaymentProfileResponse Nothing testMessages Nothing $ Just "1,1,1,This transaction has been approved.,000000,Y,2000000003,none,Test transaction for ValidateCustomerPaymentProfile.,0.01,CC,auth_only,custId123,John,Doe,,123 Main St.,Bellevue,WA,98004,USA,000-000-0000,,mark@example.com,John,Doe,,123 Main St.,Bellevue,WA,98004,USA,0.00,0.00,0.00,,none,D18EB6B211FE0BBF556B271FDA6F92EE,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,"
  in assertEncodes text response

test_updateCustomerPaymentProfileResponse :: Assertion
test_updateCustomerPaymentProfileResponse =
  let text = [r|
<?xml version="1.0" encoding="utf-8"?>
<updateCustomerPaymentProfileResponse xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <messages>
    <resultCode>Ok</resultCode>
    <message>
      <code>I00001</code>
      <text>Successful.</text>
    </message>
  </messages>
</updateCustomerPaymentProfileResponse>               
|]
      response = UpdateCustomerPaymentProfileResponse Nothing testMessages Nothing Nothing
  in assertEncodes text response

test_deleteCustomerPaymentProfileResponse :: Assertion
test_deleteCustomerPaymentProfileResponse =
  let text = [r|
<?xml version="1.0" encoding="utf-8"?>
<deleteCustomerPaymentProfileResponse xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <messages>
    <resultCode>Ok</resultCode>
    <message>
      <code>I00001</code>
      <text>Successful.</text>
    </message>
  </messages>
</deleteCustomerPaymentProfileResponse>               
|]
      response = DeleteCustomerPaymentProfileResponse Nothing testMessages Nothing
  in assertEncodes text response
test_createCustomerProfileFromTransactionResponse :: Assertion
test_createCustomerProfileFromTransactionResponse =
  let text = [r|
<?xml version="1.0" encoding="utf-8"?> 
<createCustomerProfileResponse xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <messages>
    <resultCode>Ok</resultCode>
    <message>
      <code>I00001</code>
      <text>Successful.</text>
    </message>
  </messages>
  <customerProfileId>1234</customerProfileId>
  <customerPaymentProfileIdList>
    <numericString>5678</numericString>
  </customerPaymentProfileIdList>
  <customerShippingAddressIdList>
    <numericString>1212</numericString>
  </customerShippingAddressIdList>
  <validationDirectResponseList />
</createCustomerProfileResponse>               
|]
      response = CreateCustomerProfileResponse Nothing testMessages Nothing (Just 1234) [ 5678 ] [ 1212 ] (ArrayOfString [])
  in assertEncodes text response

test_chargeCustomerProfile :: Assertion
test_chargeCustomerProfile =
  let text = [r|
<?xml version="1.0" encoding="utf-8"?>
<createTransactionResponse xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <refId>123456</refId>
  <messages>
    <resultCode>Ok</resultCode>
    <message>
      <code>I00001</code>
      <text>Successful.</text>
    </message>
  </messages>
  <transactionResponse>
    <responseCode>1</responseCode>
    <authCode>UGELQC</authCode>
    <avsResultCode>E</avsResultCode>
    <transId>2148061808</transId>
    <transHash>0B428D8A928AAC61121AF2F6EAC5FF3F</transHash>
    <testRequest>0</testRequest>
    <accountNumber>XXXX0015</accountNumber>
    <accountType>MasterCard</accountType>
    <message>
      <code>1</code>
      <description>This transaction has been approved.</description>
    </message>
    <userFields>
      <userField>
        <name>MerchantDefinedFieldName1</name>
        <value>MerchantDefinedFieldValue1</value>
      </userField>
      <userField>
        <name>favorite_color</name>
        <value>lavender</value>
      </userField>
    </userFields>
  </transactionResponse>
</createTransactionResponse>               
|]
      transactionResponse = mkTransactionResponse {
        transactionResponse_responseCode = Just "1",
        transactionResponse_authCode = Just "UGELQC",
        transactionResponse_avsResultCode = Just "E",
        transactionResponse_transId = Just "2148061808",
        transactionResponse_transHash = Just "0B428D8A928AAC61121AF2F6EAC5FF3F",
        transactionResponse_testRequest = Just "0",
        transactionResponse_accountNumber = Just "XXXX0015",
        transactionResponse_accountType = Just "MasterCard",
        transactionResponse_message = Just $ TransactionResponse_message (Just "1") (Just "This transaction has been approved."),
        transactionResponse_userFields = Just $ [
            UserField "MerchantDefinedFieldName1" "MerchantDefinedFieldValue1",
            UserField "favorite_color" "lavender"
        ]
      }
      response = CreateTransactionResponse (Just "123456") testMessages Nothing transactionResponse Nothing
  in assertEncodes text response

test_getHostedProfilePageResponse :: Assertion
test_getHostedProfilePageResponse =
  let text = [r|
<?xml version="1.0" encoding="utf-8"?>
<getHostedProfilePageResponse xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <messages>
    <resultCode>Ok</resultCode>
    <message>
      <code>I00001</code>
      <text>Successful.</text>
    </message>
  </messages>
  <token>+ZeWDaUOPoQPRGTHcKd7DYbMfcAFDrhO8GPOFNt+ACzJnvkz+aWO0SYSAA9x602jAIKKfUHUt2ybwQRaG8LzHluuR5dRgsuh+kjarKvD0hpieGjLHmnz0LHmFv1Xe9P3zpmawqBCSB/d4jcSg9dAxecNBUzMwIuYzY+vGUGLUXgr9QPaRh93HqWZrV4Mbwop</token>
</getHostedProfilePageResponse>               
|]
      response = GetHostedProfilePageResponse Nothing testMessages Nothing $ Just "+ZeWDaUOPoQPRGTHcKd7DYbMfcAFDrhO8GPOFNt+ACzJnvkz+aWO0SYSAA9x602jAIKKfUHUt2ybwQRaG8LzHluuR5dRgsuh+kjarKvD0hpieGjLHmnz0LHmFv1Xe9P3zpmawqBCSB/d4jcSg9dAxecNBUzMwIuYzY+vGUGLUXgr9QPaRh93HqWZrV4Mbwop"
  in assertEncodes text response

test_aNetApiResponse :: Assertion
test_aNetApiResponse =
  let text = [r|
<?xml version="1.0" encoding="utf-8"?>
<getCustomerProfileResponse xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns="AnetApi/xml/v1/schema/AnetApiSchema.xsd">
  <messages>
    <resultCode>Error</resultCode>
    <message>
      <code>E00040</code>
      <text>The record cannot be found.</text>
    </message>
  </messages>
</getCustomerProfileResponse>
|]
      messages = Messages Message_Error $ ArrayOf [ Message "E00040" "The record cannot be found." ]
      actual = ANetApiResponse Nothing messages Nothing
  in assertEncodesWithOptions (XmlParseOptions $ Just "getCustomerProfileResponse") text actual
                
responseTests :: TestTree
responseTests = testGroup "API Responses Encode and Decode to JSON correctly" [
      testCase "authenticateTestResponse" test_authenticateTestResponse,
      testCase "createCustomerProfileResponse" test_createCustomerProfileResponse,
      testCase "getCustomerProfileResponse" test_getCustomerProfileResponse,
      testCase "getCustomerProfileResponse2" test_getCustomerProfileResponse2,
      testCase "getCustomerProfileIdsResponse" test_getCustomerProfileIdsResponse,
      testCase "updateCustomerProfileResponse" test_updateCustomerProfileResponse,
      testCase "deleteCustomerProfileResponse" test_deleteCustomerProfileResponse,
      testCase "createCustomerPaymentProfileResponse" test_createCustomerPaymentProfileResponse,
      testCase "getCustomerPaymentProfileResponse" test_getCustomerPaymentProfileResponse,
      testCase "getCustomerPaymentProfileListResponse" test_getCustomerPaymentProfileListResponse,
      testCase "validateCustomerPaymentProfileResponse" test_validateCustomerPaymentProfileResponse,
      testCase "updateCustomerPaymentProfileResponse" test_updateCustomerPaymentProfileResponse,
      testCase "deleteCustomerPaymentProfileResponse" test_deleteCustomerPaymentProfileResponse,
      testCase "createCustomerProfileFromTransactionResponse" test_createCustomerProfileFromTransactionResponse,
      testCase "getHostedProfilePageResponse" test_getHostedProfilePageResponse,
      testCase "chargeCustomerProfile" test_chargeCustomerProfile,
      testCase "aNetApiResponse" test_aNetApiResponse
     ]
