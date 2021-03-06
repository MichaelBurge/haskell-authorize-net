{-# LANGUAGE OverloadedStrings #-}

module Test.Network.AuthorizeNet.Util where

import Data.Attoparsec.ByteString
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Test.Tasty.HUnit
import System.IO

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Network.AuthorizeNet.Instances
import Network.AuthorizeNet.Types
import Network.AuthorizeNet.Util

testMerchantAuthentication :: MerchantAuthentication
testMerchantAuthentication = MerchantAuthentication {
  merchantAuthentication_name           = "API_LOGIN_ID",
  merchantAuthentication_transactionKey = "API_TRANSACTION_KEY"
  }

testCustomerProfile :: CustomerProfile
testCustomerProfile = CustomerProfile {
  customerProfile_merchantCustomerId = "Merchant_Customer_ID",
  customerProfile_description        = "Profile description here",
  customerProfile_email              = "customer-profile-email@here.com",
  customerProfile_paymentProfiles    = Just $ CustomerPaymentProfile {
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
   customerProfile_shipTos = Nothing
  }

assertEncodesWithOptions :: (XmlParsable a, Eq a, Show a) => XmlParseOptions ->  String -> a -> Assertion
assertEncodesWithOptions options expectedS actual = do
  let actualBsl = runSchemaTypeToXmlWithOptions options actual
      expectedBsl = TL.encodeUtf8 $ TL.pack expectedS
      eExpected = runParseSchemaTypeWithOptions options expectedBsl
  case eExpected of
    Left e -> error $ "Error parsing '" <> expectedS <> "': " <> show e
    Right expected -> do
      let whitespace = map (fromIntegral . ord) ['\n', ' ']
          stripWhitespace bsl = BSL.filter (not . flip elem whitespace) bsl
          normalizeQuotes bsl = flip BSL.map bsl $ \b -> case b of
            x | x == (fromIntegral $ ord '"') -> fromIntegral $ ord '\''
            x -> x
          normalize = normalizeQuotes . stripWhitespace
      assertEqual "Encoding" (normalize expectedBsl) (normalize actualBsl)
      assertEqual "Decoding" expected actual

assertEncodes :: (XmlParsable a, Eq a, Show a) => String -> a -> Assertion
assertEncodes = assertEncodesWithOptions defaultOptions

assertDecodes :: a -> String -> Assertion
assertDecodes _ _ = return ()

-- assertEncodes :: (FromJSON a, ToJSON a, Eq a, Show a) => String -> a -> Assertion
-- assertEncodes expectedS actual = do
--   let -- Strip characters like newlines and spaces that don't matter
--       expectedSStripped = filter (not . flip elem ("\n " :: String)) expectedS :: String
--       expectedBs = TL.encodeUtf8 $ TL.pack $ expectedSStripped
--       eExpected = (`asTypeOf` actual) <$> eitherDecode expectedBs
--       actualBs = encode actual

--   case eExpected of
--     Left e -> error $ "Error parsing '" <> expectedS <> "': " <> show e
--     Right expected -> do
--       let actualJson = toJSON actual
--       hPutStrLn stderr $ show actualBs
--       assertEqual "Encoding" expectedBs actualBs
--       assertEqual "Decoding" expected actual

-- assertDecodes :: (FromJSON a, ToJSON a, Eq a, Show a) => a -> String -> Assertion
-- assertDecodes dummy xS =
--   let x = fromJust $ decode $ TL.encodeUtf8 $ TL.pack xS
--   in assertEncodes xS $ x `asTypeOf` dummy
