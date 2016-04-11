{-# LANGUAGE EmptyDataDecls, GADTs, StandaloneDeriving, OverloadedStrings, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TemplateHaskell, GeneralizedNewtypeDeriving #-}

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
import Data.String
import GHC.Generics
import Network.Wreq hiding (Proxy)

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

import Network.AuthorizeNet.TH

-- | Information about the Authorize.NET API's endpoint. See 'API Endpoints' at http://developer.authorize.net/api/reference/index.html
-- | If you had a mock of their API set up somewhere for unit tests, you would use it by creating a value of this type.
-- | This and 'MerchantAuthentication' are required for every request
data ApiConfig = ApiConfig {
  apiConfig_baseUrl :: T.Text
  } deriving (Show)


newtype NumericString = NumericString Int deriving (Eq, Show, Num)
newtype Decimal = Decimal T.Text deriving (Eq, Show, IsString, ToJSON, FromJSON)

type CustomerProfileId = NumericString
type CustomerPaymentProfileId = NumericString
type CustomerShippingAddressId = NumericString
type ShippingProfileId = NumericString
type TransactionId = NumericString
type TaxId = T.Text

type CardCode = NumericString

instance FromJSON NumericString where
  parseJSON = withText "numericString" $ \t ->
    case T.decimal t of
      Left e -> fail e
      Right (x, "") -> return $ NumericString x
      Right (x, remainder) -> fail $ T.unpack $ "Additional text found: " <> remainder

instance ToJSON NumericString where
  toJSON (NumericString x) = String $ T.pack $ show x

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
  creditCard_cardCode       :: Maybe NumericString
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

-- | anet:transactionTypeEnum
data TransactionType = Transaction_authOnlyTransaction
                     | Transaction_authCaptureTransaction
                     | Transaction_captureOnlyTransaction
                     | Transaction_refundTransaction
                     | Transaction_priorAuthCaptureTransaction
                     | Transaction_voidTransaction
                     | Transaction_getDetailsTransaction
                     | Transaction_authOnlyContinueTransaction
                     | Transaction_authCaptureContinueTransaction
                     deriving (Eq, Show)

$(deriveJSON enumType ''TransactionType)                              

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
  creditCardTrack_cardCode :: Maybe CardCode
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

$(deriveJSON choiceType ''Payment)

-- | anet:paymentProfile
data PaymentProfile = PaymentProfile {
  paymentProfile_paymentProfileId :: NumericString,
  paymentProfile_cardCode         :: Maybe CardCode
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''PaymentProfile)
                      
-- | anet:customerPaymentProfileType
data CustomerPaymentProfile = CustomerPaymentProfile {
  customerPaymentProfile_customerType   :: Maybe CustomerType,
  customerPaymentProfile_billTo         :: Maybe CustomerAddress,
  customerPaymentProfile_payment        :: Maybe Payment,
  customerPaymentProfile_driversLicense :: Maybe DriversLicense,
  customerPaymentProfile_taxId          :: Maybe TaxId
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''CustomerPaymentProfile)

mkCustomerPaymentProfile :: CustomerPaymentProfile
mkCustomerPaymentProfile = CustomerPaymentProfile Nothing Nothing Nothing Nothing Nothing

-- | anet:customerPaymentProfileTypeEx
data CustomerPaymentProfileEx = CustomerPaymentProfileEx {
  customerPaymentProfileEx_customerType             :: Maybe CustomerType,
  customerPaymentProfileEx_billTo                   :: Maybe CustomerAddress,
  customerPaymentProfileEx_payment                  :: Maybe Payment,
  customerPaymentProfileEx_driversLicense           :: Maybe DriversLicense,
  customerPaymentProfileEx_taxId                    :: Maybe TaxId,
  customerPaymentProfileEx_customerPaymentProfileId :: Maybe CustomerPaymentProfileId
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''CustomerPaymentProfileEx)

-- | anet:CustomerPaymentProfileSearchTypeEnum
data CustomerPaymentProfileSearchType = SearchType_cardsExpiringInMonth deriving (Eq, Show)

$(deriveJSON enumType ''CustomerPaymentProfileSearchType)

data CustomerPaymentProfileOrderFieldEnum = OrderField_id deriving (Eq, Show)

$(deriveJSON enumType ''CustomerPaymentProfileOrderFieldEnum)

-- | anet:CustomerPaymentProfileSorting
data CustomerPaymentProfileSorting = CustomerPaymentProfileSorting {
  customerPaymentProfileSorting_orderBy         :: CustomerPaymentProfileOrderFieldEnum,
  customerPaymentProfileSorting_orderDescending :: Bool
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''CustomerPaymentProfileSorting)

-- | anet:Paging
data Paging = Paging {
  paging_limit :: NumericString,
  paging_offset :: NumericString
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''Paging)

-- | anet:customerProfileBaseType
data CustomerProfileBase = CustomerProfileBase {
  customerProfileBase_merchantCustomerId :: Maybe T.Text,
  customerProfileBase_description        :: Maybe T.Text,
  customerProfileBase_email              :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''CustomerProfileBase)                          
  
-- | anet:customerProfileType
-- | Contains a 'Maybe' 'PaymentProfile' and 'Maybe' 'CustomerAddress' instead of an unbounded list of these due to JSON not supporting duplicate keys.
data CustomerProfile = CustomerProfile {
  customer_customerProfileId  :: Maybe CustomerProfileId,
  customer_merchantCustomerId :: T.Text,
  customer_description        :: T.Text,
  customer_email              :: T.Text,
  customer_paymentProfiles    :: Maybe CustomerPaymentProfile,
  customer_shipTos            :: Maybe CustomerAddress
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''CustomerProfile)

data ValidationMode = Validation_none
                    | Validation_testMode
                    | Validation_liveMode
                    -- | Per Authorize.NET: "NOT RECOMMENDED. Use of this option can result in fines from your processor."
                    | Validation_oldLiveMode
                    deriving (Eq, Show)

$(deriveJSON enumType ''ValidationMode)

-- | anet:customerProfilePaymentType
data CustomerProfilePayment = CustomerProfilePayment {
  customerProfilePayment_createProfile     :: Maybe Bool,
  customerProfilePayment_customerProfileId :: Maybe CustomerProfileId,
  customerProfilePayment_paymentProfile    :: Maybe PaymentProfile,
  customerProfilePayment_shippingProfileId :: Maybe ShippingProfileId
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''CustomerProfilePayment)

mkCustomerProfilePayment :: CustomerProfilePayment
mkCustomerProfilePayment = CustomerProfilePayment Nothing Nothing Nothing Nothing

-- | anet:solutionType
data Solution = Solution {
  solution_id         :: T.Text,
  solution_name       :: Maybe T.Text,
  solution_vendorName :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''Solution)

-- | anet:orderType
data Order = Order {
  order_invoiceNumber :: Maybe T.Text,
  order_description   :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''Order)

-- | anet:lineItemType
data LineItem = LineItem {
  lineItem_itemId      :: T.Text,
  lineItem_name        :: T.Text,
  lineItem_description :: Maybe T.Text,
  lineItem_quantity    :: Decimal,
  lineItem_unitPrice   :: Decimal,
  lineItem_taxable     :: Maybe Bool
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''LineItem)

-- | anet:ArrayOfLineItem
data LineItems = LineItems {
  lineItems_lineItem :: LineItem
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''LineItems)                 

-- | anet:extendedAmountType
data ExtendedAmount = ExtendedAmount {
  extendedAmount_amount      :: Decimal,
  extendedAmount_name        :: Maybe T.Text,
  extendedAmount_description :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''ExtendedAmount)

-- | anet:customerDataType
data CustomerData = CustomerData {
  customerData_type            :: Maybe CustomerType,
  customerData_id              :: Maybe T.Text,
  customerData_email           :: Maybe T.Text,
  customerData_driverseLicense :: Maybe DriversLicense,
  customerData_taxId           :: Maybe TaxId
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''CustomerData)

-- | anet:ccAuthenticationType
data CcAuthentication = CcAuthentication {
  ccAuthentication_authenticationIndicator       :: T.Text,
  ccAuthentication_cardholderAuthenticationValue :: T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''CcAuthentication)

-- | anet:transRetailInfoType
data TransRetailInfo = TransRetailInfo {
  transRetailInfo_marketType        :: Maybe T.Text,
  transRetailInfo_deviceType        :: Maybe T.Text,
  transRetailInfo_customerSignature :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''TransRetailInfo)

data SettingName = SettingName_emailCustomer
                 | SettingName_merchantEmail
                 | SettingName_allowPartialAuth
                 | SettingName_headerEmailReceipt
                 | SettingName_footerEmailReceipt
                 | SettingName_recurringBilling
                 | SettingName_duplicateWindow
                 | SettingName_testRequest
                 | SettingName_hostedProfileReturnUrl
                 | SettingName_hostedProfileReturnUrlText
                 | SettingName_hostedProfilePageBorderVisible
                 | SettingName_hostedProfileIFrameCommunicatorUrl
                 | SettingName_hostedProfileHeadingBgColor
                 | SettingName_hostedProfileValidationMode
                 | SettingName_hostedProfileBillingAddressRequired
                 | SettingName_hostedProfileCardCodeRequired
                 deriving (Eq, Show)

$(deriveJSON enumType ''SettingName)


-- | anet:settingType
data Setting = Setting {
  setting_settingName  :: SettingName,
  setting_settingValue :: T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''Setting)

-- | anet:userField
data UserField = UserField {
  userField_name  :: T.Text,
  userField_value :: T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''UserField)

data SecureAcceptance = SecureAcceptance {
  secureAcceptance_SecureAcceptanceUrl :: T.Text,
  secureAcceptance_PayerID             :: T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''SecureAcceptance)

data EmvResponse = EmvResponse {
  emvResponse_tsvData :: Maybe T.Text,
  emvResponse_tag     :: Maybe T.Text
  } deriving (Eq, Show)

-- | anet:transactionRequestType
data TransactionRequest = TransactionRequest {
  transactionRequest_transactionType          :: TransactionType,
  transactionRequest_amount                   :: Decimal,
  transactionRequest_currencyCode             :: Maybe T.Text,
  transactionRequest_payment                  :: Maybe Payment,
  transactionRequest_profile                  :: Maybe CustomerProfilePayment,
  transactionRequest_solution                 :: Maybe Solution,
  transactionRequest_callId                   :: Maybe T.Text,
  transactionRequest_terminalNumber           :: Maybe T.Text,
  transactionRequest_authCode                 :: Maybe T.Text,
  transactionRequest_refTransId               :: Maybe T.Text,
  transactionRequest_splitTenderId            :: Maybe T.Text,
  transactionRequest_order                    :: Maybe Order,
  transactionRequest_lineItems                :: Maybe LineItems,
  transactionRequest_tax                      :: Maybe ExtendedAmount,
  transactionRequest_duty                     :: Maybe ExtendedAmount,
  transactionRequest_shipping                 :: Maybe ExtendedAmount,
  transactionRequest_taxExempt                :: Maybe Bool,
  transactionRequest_poNumber                 :: Maybe T.Text,
  transactionRequest_customer                 :: Maybe CustomerData,
  transactionRequest_billTo                   :: Maybe CustomerAddress,
  transactionRequest_shipTo                   :: Maybe CustomerAddress,
  transactionRequest_customerIP               :: Maybe T.Text,
  transactionRequest_cardholderAuthentication :: Maybe CcAuthentication,
  transactionRequest_retail                   :: Maybe TransRetailInfo,
  transactionRequest_employeeId               :: Maybe T.Text,
  transactionRequest_transactionSettings      :: Maybe [Setting],
  transactionRequest_userFields               :: Maybe UserField
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''TransactionRequest)

-- | The TransactionRequest type has a lot of Maybe fields, so use this to get a bare-bones default.
mkTransactionRequest :: TransactionType -> Decimal -> TransactionRequest
mkTransactionRequest transactionType amount = TransactionRequest transactionType amount Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

extract :: FromJSON a => T.Text -> Value -> Parser a
extract member value = withObject (T.unpack member) (.: member) value

mExtract :: FromJSON a => T.Text -> Value -> Parser (Maybe a)
mExtract member value = withObject (T.unpack member) (.:? member) value

-- anet:messageTypeEnum
data MessageType = Message_Ok
                 | Message_Error
                 deriving (Eq, Show)
$(deriveJSON enumType ''MessageType)                          

-- | The possible message codes are documented at http://developer.authorize.net/api/reference/dist/json/responseCodes.json
data Message = Message {
  message_code :: T.Text,
  message_text :: T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''Message)

-- anet:messagesType
data Messages = Messages {
  messages_resultCode :: MessageType,
  messages_message    :: [Message]
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''Messages)
  
-- data ApiError = ErrorResponseDecoding T.Text
--               deriving (Show)

-- makeApiRequest :: (ToJSON a, FromJSON (ApiResponse a)) => ApiConfig -> a -> EitherT ApiError IO (ApiResponse a)
-- makeApiRequest apiConfig apiRequest = do
--   let baseUrl = T.unpack $ apiConfig_baseUrl apiConfig
--       requestBs = encode apiRequest
--   response <- liftIO $ post baseUrl requestBs
--   case eitherDecode $ response ^. responseBody of
--     Left e -> throwError $ ErrorResponseDecoding $ T.pack e
--     Right x -> return x
