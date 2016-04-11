{-# LANGUAGE EmptyDataDecls, GADTs, StandaloneDeriving, OverloadedStrings, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TemplateHaskell #-}

-- | Implements the Authorize.NET JSON API. Types generally correspond to those defined in the XSD.
-- | XSD location: https://api.authorize.net/xml/v1/schema/AnetApiSchema.xsd
module Network.AuthorizeNet.Api where

import Control.Applicative
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Proxy
import GHC.Generics
import Network.Wreq hiding (Proxy)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Network.AuthorizeNet.TH

-- | Information about the Authorize.NET API's endpoint. See 'API Endpoints' at http://developer.authorize.net/api/reference/index.html
-- | If you had a mock of their API set up somewhere for unit tests, you would use it by creating a value of this type.
-- | This and 'MerchantAuthentication' are required for every request
data ApiConfig = ApiConfig {
  apiConfig_baseUrl :: T.Text
  } deriving (Show)


type CustomerId = T.Text
type PaymentProfileId = Int

type NumericString = T.Text

-- | The sandbox endpoint for Authorize.NET
sandboxApiConfig :: ApiConfig
sandboxApiConfig = ApiConfig {
  apiConfig_baseUrl = "https://apitest.authorize.net/xml/v1/request.api"
  }

-- | The production endpoint for Authorize.NET
productionApiConfig :: ApiConfig
productionApiConfig = ApiConfig {
  apiConfig_baseUrl = "https://api.authorize.net/xml/v1/request.api"
  }

-- | Holds API credentials for Authorize.NET. You should get these when you sign up for a sandbox or production account.
-- | This and 'ApiConfig' are required for every request.
data MerchantAuthentication = MerchantAuthentication {
  merchantAuthentication_name           :: T.Text,
  merchantAuthentication_transactionKey :: T.Text
  } deriving (Eq)

instance Show MerchantAuthentication where
  show x = "MerchantAuthentication { merchantAuthentication_name = \"REDACTED\", merchantAuthentication_transactionKey = \"REDACTED\" }"

instance ToJSON MerchantAuthentication where
  toJSON (MerchantAuthentication name transactionKey) = object [
    "name" .= name,
    "transactionKey" .= transactionKey
    ]

instance FromJSON MerchantAuthentication where
  parseJSON value =
    let objectParse o = MerchantAuthentication <$> o .: "name" <*> o .: "transactionKey"
    in withObject "MerchantAuthentication" objectParse value

data AvsResult = AvsResult {
  avsResultCode        :: Maybe Int,
  avsResultMessage     :: Maybe T.Text,
  avsResultStreetMatch :: Maybe T.Text,
  avsResultPostalMatch :: Maybe T.Text
  } deriving (Eq, Show)

data CvvResult = CvvResult {
  cvvResultCode    :: Maybe Int,
  cvvResultMessage :: Maybe T.Text
  } deriving (Eq, Show)


data GatewayResponse = GatewayResponse {
  gatewayErrorCode     :: Maybe Int,
  gatewayMessage       :: T.Text,
  gatewayAuthorization :: Maybe T.Text,
  gatewayAvsResult     :: Maybe AvsResult,
  gatewayCvvResult     :: Maybe CvvResult,
  gatewayParams        :: Maybe Object,
  gatewayIsTest        :: Bool
  } deriving (Eq, Show)

-- data Customer = Customer {
--   customerProfile         :: [CustomerProfile]
--   } deriving (Eq, Show)

-- | anet:customerTypeEnum
data CustomerType = CustomerType_individual
                  | CustomerType_business
                  deriving (Eq, Show)

$(deriveJSON enumType ''CustomerType)

(.=?) :: (ToJSON a) => T.Text -> Maybe a -> Maybe Pair
(.=?) field value = (field .=) <$> value

-- | anet:nameAndAddressType
data NameAndAddress = NameAndAddress {
  nameAddress_firstName   :: T.Text,
  nameAddress_lastName    :: T.Text,
  nameAddress_company     :: T.Text,
  nameAddress_address     :: T.Text,
  nameAddress_city        :: T.Text,
  nameAddress_state       :: T.Text,
  nameAddress_zip         :: T.Text,
  nameAddress_country     :: T.Text
  } deriving (Eq, Show)

    
data CreditCard = CreditCard {
  creditCard_cardNumber     :: T.Text,
  creditCard_expirationDate :: T.Text,
  creditCard_cardCode       :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''CreditCard)

data CustomerAddress = CustomerAddress {
  customerAddress_firstName   :: Maybe T.Text,
  customerAddress_lastName    :: Maybe T.Text,
  customerAddress_company     :: Maybe T.Text,
  customerAddress_address     :: Maybe T.Text,
  customerAddress_city        :: Maybe T.Text,
  customerAddress_state       :: Maybe T.Text,
  customerAddress_zip         :: Maybe T.Text,
  customerAddress_country     :: Maybe T.Text,
  customerAddress_phoneNumber :: Maybe T.Text,
  customerAddress_faxNumber   :: Maybe T.Text,
  customerAddress_email       :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''CustomerAddress)


-- | anet:driversLicenseType
data DriversLicense = DriversLicense {
  driversLicense_number      :: T.Text,
  driversLicense_state       :: T.Text,
  driversLicense_dateOfBirth :: T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''DriversLicense)

data BankAccountType = BankAccountType_Individual
                     | BankAccountType_Business
                     deriving (Eq, Show)

$(deriveJSON enumType ''BankAccountType)                             

-- | anet:echeckTypeEnum
data EcheckType = Echeck_PPD
                | Echeck_WEB
                | Echeck_CCD
                | Echeck_TEL
                | Echeck_ARC
                | Echeck_BOC
                deriving (Eq, Show)

$(deriveJSON enumType ''EcheckType)

-- | anet:bankAccountType
data BankAccount = BankAccount {
  bankAccount_accountType   :: Maybe BankAccountType,
  bankAccount_routingNumber :: T.Text,
  bankAccount_accountNumber :: T.Text,
  bankAccount_nameOnAccount :: T.Text,
  bankAccount_echeckType    :: Maybe EcheckType,
  bankAccount_bankName      :: Maybe T.Text,
  bankAccount_checkNumber   :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''BankAccount)

-- | anet:creditCardTrackType
data CreditCardTrack = CreditCardTrack {
  creditCardTrack_track1   :: Maybe T.Text,
  creditCardTrack_track2   :: Maybe T.Text,
  creditCardTrack_cardCode :: Maybe NumericString
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''CreditCardTrack)
                       
-- | anet:encryptedTrackDataType
data EncryptedTrackData = EncryptedTrackData deriving (Eq, Show)
$(deriveJSON dropRecordName ''EncryptedTrackData)
  
-- | anet:payPalType
data PayPal = PayPal deriving (Eq, Show)
$(deriveJSON dropRecordName ''PayPal)

-- | anet:opaqueDataType
data OpaqueData = OpaqueData deriving (Eq, Show)
$(deriveJSON dropRecordName ''OpaqueData)

-- | anet:paymentEmvType
data PaymentEmv = PaymentEmv deriving (Eq, Show)
$(deriveJSON dropRecordName ''PaymentEmv)

-- | anet:paymentType
data Payment = Payment_creditCard CreditCard
             | Payment_bankAccount BankAccount
             | Payment_trackData CreditCardTrack
             | Payment_encryptedTrackData EncryptedTrackData
             | Payment_payPal PayPal
             | Payment_opaqueData OpaqueData
             | Payment_emv PaymentEmv
             deriving (Eq, Show)
                      
-- | anet:customerPaymentProfileType
data CustomerPaymentProfile = CustomerPaymentProfile {
  customerPaymentProfile_customerType   :: Maybe CustomerType,
  customerPaymentProfile_billTo         :: Maybe CustomerAddress,
  customerPaymentProfile_payment        :: Maybe Payment,
  customerPaymentProfile_driversLicense :: Maybe DriversLicense,
  customerPaymentProfile_taxId          :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''CustomerPaymentProfile)


-- | anet:customerProfileType
-- | Contains a 'Maybe' 'PaymentProfile' and 'Maybe' 'CustomerAddress' instead of an unbounded list of these due to JSON not supporting duplicate keys.
data CustomerProfile = CustomerProfile {
  customer_customerProfileId  :: Maybe CustomerId,
  customer_merchantCustomerId :: T.Text,
  customer_description        :: T.Text,
  customer_email              :: T.Text,
  customer_paymentProfiles    :: Maybe CustomerPaymentProfile,
  customer_shipTos            :: Maybe CustomerAddress
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''CustomerProfile)

data NewCustomerRequest = NewCustomerRequest {
  newCustomerUserId   :: Int,
  newCustomerEmail    :: T.Text,
  newCustomerUsername :: T.Text
  } deriving (Eq, Show)

instance ToJSON NewCustomerRequest where
  toJSON x = object [
    "user_id" .= newCustomerUserId x,
    "email_address" .= newCustomerEmail x,
    "username" .= newCustomerUsername x
    ]

data NewCustomerResponse = NewCustomerSuccess {  newCustomerId :: Int  }
                         | NewCustomerError { newCustomerErrorMessage :: T.Text }
                         deriving (Eq, Show)

$(deriveJSON enumType ''Payment)
                      
instance FromJSON NewCustomerResponse where
  parseJSON (Object v) = do
    mAuthorization <- v .: "authorization"
    case mAuthorization of
      Nothing -> NewCustomerError <$> v .: "message"
      Just authorization -> return $ NewCustomerSuccess authorization

data AllCustomersResponse = AllCustomersResponse {
  allCustomersResponse_customerIds :: [CustomerId]
  } deriving (Show)

data UpdateCustomerRequest = UpdateCustomerRequest {
  updateCustomerRequest_customerProfileId :: CustomerId,
  updateCustomerRequest_emailAddress      :: T.Text,
  updateCustomerRequest_username          :: T.Text,
  updateCustomerRequest_userId            :: T.Text
  } deriving (Show)

data UpdateCustomerResponse = UpdateCustomerResponse deriving (Show)

data DeleteCustomerResponse = DeleteCustomerResponse deriving (Show)

extract :: FromJSON a => T.Text -> Value -> Parser a
extract member value = withObject (T.unpack member) (.: member) value

mExtract :: FromJSON a => T.Text -> Value -> Parser (Maybe a)
mExtract member value = withObject (T.unpack member) (.:? member) value

-- | The API requests are documented at http://developer.authorize.net/api/reference/index.html
data ApiRequest = AuthenticateTest {
  authenticateTest_merchantAuthentication :: MerchantAuthentication
  } | CreateCustomerProfile {
  createCustomerProfile_merchantAuthentication :: MerchantAuthentication,
  createCustomerProfile_profile                :: CustomerProfile
  } | GetCustomerProfile {
  getCustomerProfile_merchantAuthentication :: MerchantAuthentication,
  getCustomerProfile_customerProfileId      :: CustomerId
  } | GetCustomerProfileIds {
  getCustomerProfileIds_merchantAuthentication :: MerchantAuthentication
  } | UpdateCustomerProfile {
  updateCustomerProfile_merchantAuthentication :: MerchantAuthentication,
  updateCustomerProfile_profile                :: CustomerProfile
  } | DeleteCustomerProfile {
  deleteCustomerProfile_merchantAuthentication :: MerchantAuthentication,
  deleteCustomerProfile_customerProfileId      :: CustomerId
  }
  deriving (Eq, Show)

$(deriveJSON requestOptions ''ApiRequest)

data ApiError = ErrorResponseDecoding T.Text
              deriving (Show)

-- | Associates each API Request type with a response type
type family ApiResponse a where
  ApiResponse ApiRequest = ()

makeApiRequest :: (ToJSON a, FromJSON (ApiResponse a)) => ApiConfig -> a -> EitherT ApiError IO (ApiResponse a)
makeApiRequest apiConfig apiRequest = do
  let baseUrl = T.unpack $ apiConfig_baseUrl apiConfig
      requestBs = encode apiRequest
  response <- liftIO $ post baseUrl requestBs
  case eitherDecode $ response ^. responseBody of
    Left e -> throwError $ ErrorResponseDecoding $ T.pack e
    Right x -> return x
