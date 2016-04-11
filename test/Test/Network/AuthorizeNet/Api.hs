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
  customer_customerProfileId  = Nothing,
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

apiExpected_authenticateTest :: String
apiExpected_authenticateTest = [r|
{
    "authenticateTestRequest": {
        "merchantAuthentication": {
            "name": "API_LOGIN_ID",
            "transactionKey": "API_TRANSACTION_KEY"
        }
    }  
}
|]

apiActual_authenticateTest :: ApiRequest
apiActual_authenticateTest = AuthenticateTest testMerchantAuthentication

apiExpected_createCustomerProfile :: String
apiExpected_createCustomerProfile = [r|
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

apiActual_createCustomerProfile :: ApiRequest
apiActual_createCustomerProfile = CreateCustomerProfile testMerchantAuthentication testCustomerProfile

apiExpected_getCustomerProfile :: String
apiExpected_getCustomerProfile = [r|
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

apiActual_getCustomerProfile :: ApiRequest
apiActual_getCustomerProfile = GetCustomerProfile testMerchantAuthentication 10000

apiExpected_getCustomerProfileIds :: String
apiExpected_getCustomerProfileIds = [r|
{
    "getCustomerProfileIdsRequest": {
        "merchantAuthentication": {
            "name": "API_LOGIN_ID",
            "transactionKey": "API_TRANSACTION_KEY"
        }
    }
}
|]

apiActual_getCustomerProfileIds :: ApiRequest
apiActual_getCustomerProfileIds = GetCustomerProfileIds testMerchantAuthentication

apiExpected_updateCustomerProfile :: String
apiExpected_updateCustomerProfile = [r|
{
    "updateCustomerProfileRequest": {
        "merchantAuthentication": {
            "name": "API_LOGIN_ID",
            "transactionKey": "API_TRANSACTION_KEY"
        },
        "profile": {
            "merchantCustomerId": "custId123",
            "description": "some description",
            "email": "newaddress@example.com",
            "customerProfileId": "10000"
        }
    }
}
|]

apiActual_updateCustomerProfile :: ApiRequest
apiActual_updateCustomerProfile = UpdateCustomerProfile testMerchantAuthentication $ CustomerProfile {
  customer_customerProfileId = Just 10000,
  customer_merchantCustomerId = "custId123",
  customer_description = "some description",
  customer_email = "newaddress@example.com",
  customer_paymentProfiles = Nothing,
  customer_shipTos = Nothing
  }

apiExpected_deleteCustomerProfile :: String
apiExpected_deleteCustomerProfile = [r|
{
    "deleteCustomerProfileRequest": {
        "merchantAuthentication": {
            "name": "API_LOGIN_ID",
            "transactionKey": "API_TRANSACTION_KEY"
        },
        "customerProfileId": "10000"
    }
}
|]

apiActual_deleteCustomerProfile :: ApiRequest
apiActual_deleteCustomerProfile = DeleteCustomerProfile testMerchantAuthentication 10000

apiExpected_createCustomerPaymentProfile :: String
apiExpected_createCustomerPaymentProfile = [r|
{
    "createCustomerPaymentProfileRequest": {
        "merchantAuthentication": {
            "name": "API_LOGIN_ID",
            "transactionKey": "API_TRANSACTION_KEY"
        },
        "customerProfileId": "10000",
        "paymentProfile": {
            "billTo": {
                "firstName": "John",
                "lastName": "Doe",
                "company": "",
                "address": "123 Main St.",
                "city": "Bellevue",
                "state": "WA",
                "zip": "98004",
                "country": "USA",
                "phoneNumber": "000-000-0000",
                "faxNumber": ""
            },
            "payment": {
                "creditCard": {
                    "cardNumber": "4111111111111111",
                    "expirationDate": "2023-12"
                }
            }
        },
        "validationMode": "liveMode"
    }
}
|]

apiActual_createCustomerPaymentProfile :: ApiRequest
apiActual_createCustomerPaymentProfile = CreateCustomerPaymentProfile testMerchantAuthentication 10000 $ CustomerPaymentProfile {
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
      creditCard_cardCode = Nothing
      },
  customerPaymentProfile_driversLicense = Nothing,
  customerPaymentProfile_taxId = Nothing
  }

apiExpected_getCustomerPaymentProfile :: String
apiExpected_getCustomerPaymentProfile = [r|
{
    "getCustomerPaymentProfileRequest": {
        "merchantAuthentication": {
            "name": "API_LOGIN_ID",
            "transactionKey": "API_TRANSACTION_KEY"
        },
        "customerProfileId": "10000",
        "customerPaymentProfileId": "20000"
    }
}
|]

apiActual_getCustomerPaymentProfile :: ApiRequest
apiActual_getCustomerPaymentProfile = GetCustomerPaymentProfile testMerchantAuthentication 10000 20000

apiExpected_getCustomerPaymentProfileList :: String
apiExpected_getCustomerPaymentProfileList = [r|
{ 
         "getCustomerPaymentProfileListRequest": { 
                 "merchantAuthentication": { 
                         "name": "API_LOGIN_ID", 
                         "transactionKey": "API_TRANSACTION_KEY" 
                 }, 
                 "searchType": "cardsExpiringInMonth", 
                 "month": "2020-12", 
                 "sorting": { 
                         "orderBy": "id", 
                         "orderDescending": "false" 
                 }, 
                 "paging": { 
                         "limit": "1000", 
                         "offset": "1" 
                 } 
         } 
 }
|]

apiActual_getCustomerPaymentProfileList :: ApiRequest
apiActual_getCustomerPaymentProfileList =
  let searchType = SearchType_cardsExpiringInMonth
      sorting = CustomerPaymentProfileSorting OrderField_id False
      paging = Paging 1000 1
  in GetCustomerPaymentProfileList testMerchantAuthentication searchType "2020-12" sorting paging

apiExpected_validateCustomerPaymentProfile :: String
apiExpected_validateCustomerPaymentProfile = [r|
{
    "validateCustomerPaymentProfileRequest": {
        "merchantAuthentication": {
            "name": "API_LOGIN_ID",
            "transactionKey": "API_TRANSACTION_KEY"
        },
        "customerProfileId": "10000",
        "customerPaymentProfileId": "20000",
        "validationMode": "liveMode"
    }
}
|]

apiActual_validateCustomerPaymentProfile :: ApiRequest
apiActual_validateCustomerPaymentProfile = ValidateCustomerPaymentProfile testMerchantAuthentication 10000 20000 Nothing Nothing Validation_liveMode

apiExpected_updateCustomerPaymentProfile :: String
apiExpected_updateCustomerPaymentProfile = [r|
{
    "updateCustomerPaymentProfileRequest": {
        "merchantAuthentication": {
            "name": "API_LOGIN_ID",
            "transactionKey": "API_TRANSACTION_KEY"
        },
        "customerProfileId": "10000",
        "paymentProfile": {
            "billTo": {
                "firstName": "John",
                "lastName": "Doe",
                "company": "",
                "address": "123 Main St.",
                "city": "Bellevue",
                "state": "WA",
                "zip": "98004",
                "country": "USA",
                "phoneNumber": "000-000-0000",
                "faxNumber": ""
            },
            "payment": {
                "creditCard": {
                    "cardNumber": "4111111111111111",
                    "expirationDate": "2026-01"
                }
            },
            "customerPaymentProfileId": "20000"
        },
        "validationMode": "liveMode"
    }
}
|]

apiActual_updateCustomerPaymentProfile =
  let paymentProfile = CustomerPaymentProfileEx {
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
            creditCard_cardCode = Nothing
            },
        customerPaymentProfileEx_driversLicense           = Nothing,
        customerPaymentProfileEx_taxId                    = Nothing,
        customerPaymentProfileEx_customerPaymentProfileId = Just 20000
        }
  in UpdateCustomerPaymentProfile testMerchantAuthentication 10000 paymentProfile Validation_liveMode

apiExpected_deleteCustomerPaymentProfile :: String
apiExpected_deleteCustomerPaymentProfile = [r|
{
    "deleteCustomerPaymentProfileRequest": {
        "merchantAuthentication": {
            "name": "API_LOGIN_ID",
            "transactionKey": "API_TRANSACTION_KEY"
        },
        "customerProfileId": "10000",
        "customerPaymentProfileId": "20000"
    }
}
|]

apiActual_deleteCustomerPaymentProfile :: ApiRequest
apiActual_deleteCustomerPaymentProfile = DeleteCustomerPaymentProfile testMerchantAuthentication 10000 20000

apiExpected_createCustomerProfileFromTransaction :: String
apiExpected_createCustomerProfileFromTransaction = [r|
{
    "createCustomerProfileFromTransactionRequest": {
        "merchantAuthentication": {
            "name": "API_LOGIN_ID",
            "transactionKey": "API_TRANSACTION_KEY"
        },
        "transId": "122"
    }
}
|]

apiActual_createCustomerProfileFromTransaction :: ApiRequest
apiActual_createCustomerProfileFromTransaction = CreateCustomerProfileFromTransaction testMerchantAuthentication 122 Nothing Nothing

apiExpected_chargeCustomerProfile :: String
apiExpected_chargeCustomerProfile = [r|
{
    "createTransactionRequest": {
        "merchantAuthentication": {
            "name": "API_LOGIN_ID",
            "transactionKey": "API_TRANSACTION_KEY"
        },
        "refId": "123456",
        "transactionRequest": {
            "transactionType": "authCaptureTransaction",
            "amount": "45",
              "profile": {
                        "customerProfileId": "27388924",
                        "paymentProfile": { "paymentProfileId": "25000332" }
                        },
            "lineItems": {
                "lineItem": {
                    "itemId": "1",
                    "name": "vase",
                    "description": "Cannes logo",
                    "quantity": "18",
                    "unitPrice": "45.00"
                }
            }
        }
    }
}
|]

apiActual_chargeCustomerProfile :: ApiRequest
apiActual_chargeCustomerProfile =
  let transactionRequest = (mkTransactionRequest Transaction_authCaptureTransaction "45") {
        transactionRequest_profile = Just $ mkCustomerProfilePayment {
            customerProfilePayment_customerProfileId = Just 27388924,
            customerProfilePayment_paymentProfile = Just $ PaymentProfile 25000332 Nothing
            },
        transactionRequest_lineItems = Just $ LineItems $ LineItem {
            lineItem_itemId = "1",
            lineItem_name = "vase",
            lineItem_description = Just "Cannes logo",
            lineItem_quantity = "18",
            lineItem_unitPrice = "45.00",
            lineItem_taxable = Nothing
            }
        }
  in CreateTransaction testMerchantAuthentication (Just "123456") transactionRequest
     
authorizeNetTests :: TestTree
authorizeNetTests = testGroup "API Requests Encode to JSON" [
  testCase "authenticateTestRequest" $ assertEncodes apiExpected_authenticateTest apiActual_authenticateTest,
  testCase "createCustomerProfileRequest" $ assertEncodes apiExpected_createCustomerProfile apiActual_createCustomerProfile,
  testCase "getCustomerProfileRequest" $ assertEncodes apiExpected_getCustomerProfile apiActual_getCustomerProfile,
  testCase "getCustomerProfileIdsRequest" $ assertEncodes apiExpected_getCustomerProfileIds apiActual_getCustomerProfileIds,
  testCase "updateCustomerProfileRequest" $ assertEncodes apiExpected_updateCustomerProfile apiActual_updateCustomerProfile,
  testCase "deleteCustomerProfileRequest" $ assertEncodes apiExpected_deleteCustomerProfile apiActual_deleteCustomerProfile,
  testCase "createCustomerPaymentProfile" $ assertEncodes apiExpected_createCustomerPaymentProfile apiActual_createCustomerPaymentProfile,
  testCase "getCustomerPaymentProfile" $ assertEncodes apiExpected_getCustomerPaymentProfile apiActual_getCustomerPaymentProfile,
  testCase "validateCustomerPaymentProfile" $ assertEncodes apiExpected_validateCustomerPaymentProfile apiActual_validateCustomerPaymentProfile,
  testCase "updateCustomerPaymentProfile" $ assertEncodes apiExpected_updateCustomerPaymentProfile apiActual_updateCustomerPaymentProfile,
  testCase "deleteCustomerPaymentProfile" $ assertEncodes apiExpected_deleteCustomerPaymentProfile apiActual_deleteCustomerPaymentProfile,
  testCase "createCustomerProfileFromTransaction" $ assertEncodes apiExpected_createCustomerProfileFromTransaction apiActual_createCustomerProfileFromTransaction,
  testCase "chargeCustomerProfile" $ assertEncodes apiExpected_chargeCustomerProfile apiActual_chargeCustomerProfile
  ]
