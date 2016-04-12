{-# LANGUAGE OverloadedStrings #-}

module Test.Network.AuthorizeNet.Util where

import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Network.AuthorizeNet.Types

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
          creditCard_cardCode = Nothing,
          creditCard_isPaymentToken = Nothing,
          creditCard_cryptogram = Nothing
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

assertDecodes :: (FromJSON a, ToJSON a) => a -> String -> Assertion
assertDecodes dummy xS =
  let x = fromJust $ decode $ TL.encodeUtf8 $ TL.pack xS
  in assertEncodes xS $ x `asTypeOf` dummy
