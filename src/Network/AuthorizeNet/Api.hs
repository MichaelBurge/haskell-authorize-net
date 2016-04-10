{-# LANGUAGE GADTs, StandaloneDeriving, OverloadedStrings, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies #-}

module Network.AuthorizeNet.Api where

import Control.Applicative
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Error
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid
import Data.Proxy
import GHC.Generics
import Network.Wreq hiding (Proxy)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- | Information about the Authorize.NET API's endpoint. See 'API Endpoints' at http://developer.authorize.net/api/reference/index.html
-- | If you had a mock of their API set up somewhere for unit tests, you would use it by creating a value of this type.
-- | This and 'MerchantAuthentication' are required for every request
data ApiConfig = ApiConfig {
  apiConfig_baseUrl :: T.Text
  } deriving (Show)

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
  }

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

data Customer = Customer {
  customerProfile         :: [CustomerProfile],
  customerPaymentProfiles :: [PaymentProfile]
  } deriving (Eq, Show)

data CustomerProfile = CustomerProfile {
  customerId       :: CustomerId,
  customerEmail    :: T.Text,
  customerUsername :: T.Text,
  customerUserId   :: Int
  } deriving (Eq, Show)

type CustomerId = Int
type PaymentProfileId = Int

data NameAndAddress = NameAndAddress {
  nameAddress_firstName   :: T.Text,
  nameAddress_lastName    :: T.Text,
  nameAddress_company     :: T.Text,
  nameAddress_address     :: T.Text,
  nameAddress_city        :: T.Text,
  nameAddress_state       :: T.Text,
  nameAddress_zip         :: T.Text,
  nameAddress_country     :: T.Text,
  nameAddress_phoneNumber :: T.Text,
  nameAddress_faxNumber   :: T.Text
  } deriving (Eq, Show)

data CreditCard = CreditCard {
  creditCard_cardNumber     :: T.Text,
  creditCard_expirationDate :: T.Text,
  creditCard_cardCode       :: Maybe T.Text
  } deriving (Eq, Show)

data PaymentProfile = PaymentProfile {
  paymentProfile_paymentProfileId :: PaymentProfileId,
  paymentProfile_billTo           :: NameAndAddress,
  paymentProfile_creditCard       :: CreditCard
  } deriving (Eq, Show)

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

class ApiRequest a where
  type ApiResponse a :: *
  mkRequest :: MerchantAuthentication -> a


data UpdateCustomerResponse = UpdateCustomerResponse deriving (Show)

data DeleteCustomerResponse = DeleteCustomerResponse deriving (Show)

data AuthenticateTestRequest = AuthenticateTestRequest MerchantAuthentication deriving (Show)

extract :: FromJSON a => T.Text -> Value -> Parser a
extract member value = withObject (T.unpack member) (.: member) value

instance FromJSON AuthenticateTestRequest where
  parseJSON value =
    let merchantAuthentication = do
          inner <- extract "authenticateTestRequest" value
          extract "merchantAuthentication" inner
    in AuthenticateTestRequest <$> merchantAuthentication

instance ToJSON AuthenticateTestRequest where
  toJSON (AuthenticateTestRequest merchantAuthentication) = object [
    "authenticateTestRequest" .= object [
        "merchantAuthentication" .= toJSON merchantAuthentication
        ]
    ]

data ApiError = ErrorResponseDecoding T.Text
              deriving (Show)

makeApiRequest :: (ToJSON a, FromJSON b, ApiRequest a) => ApiConfig -> a -> EitherT ApiError IO b
makeApiRequest apiConfig apiRequest = do
  let baseUrl = T.unpack $ apiConfig_baseUrl apiConfig
      requestBs = encode apiRequest
  response <- liftIO $ post baseUrl requestBs
  case eitherDecode $ response ^. responseBody of
    Left e -> throwError $ ErrorResponseDecoding $ T.pack e
    Right x -> return x
