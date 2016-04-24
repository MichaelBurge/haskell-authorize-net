{-# LANGUAGE EmptyDataDecls, GADTs, StandaloneDeriving, OverloadedStrings, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveFoldable, OverloadedLists, DeriveGeneric #-}

-- | Implements the Authorize.NET JSON API. Types generally correspond to those defined in the XSD.
-- | XSD location: https://api.authorize.net/xml/v1/schema/AnetApiSchema.xsd
module Network.AuthorizeNet.Types where

import Control.Applicative
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types hiding (Parser)
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.String
import GHC.Generics
import GHC.Exts
import Network.Wreq hiding (Proxy)
import Text.XML.HaXml.Schema.Schema (SchemaType(..), SimpleType(..), Extension(..), Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

import Network.AuthorizeNet.Instances
import Network.AuthorizeNet.TH

-- | Information about the Authorize.NET API's endpoint. See 'API Endpoints' at http://developer.authorize.net/api/reference/index.html
-- | If you had a mock of their API set up somewhere for unit tests, you would use it by creating a value of this type.
-- | This and 'MerchantAuthentication' are required for every request
data ApiConfig = ApiConfig {
  apiConfig_baseUrl :: String,
  apiConfig_hostedProfileUrl :: T.Text
  } deriving (Show)


newtype NumericString = NumericString Int deriving (Eq, Ord, Show, Num)
newtype Decimal = Decimal T.Text deriving (Eq, Show, IsString, ToJSON, FromJSON)
-- | Some Authorize.NET services in their JSON represent a single element as a single-element list, and others use an object. This type normalizes them into a list.
data ArrayOf a = ArrayOf [a] deriving (Eq, Show, Foldable)

instance SchemaType a => SchemaType (ArrayOf a) where
  parseSchemaType s = return ArrayOf `apply` many (parseSchemaType s)

instance FromJSON a => FromJSON (ArrayOf a) where
  parseJSON value = case value of
    Array _ -> ArrayOf <$> parseJSON value
    _ -> ArrayOf <$> pure <$> parseJSON value

instance ToJSON a => ToJSON (ArrayOf a) where
  toJSON (ArrayOf xs) = toJSON xs

instance IsList (ArrayOf a) where
  type Item (ArrayOf a) = a
  fromList xs = ArrayOf xs
  toList (ArrayOf xs) = xs

type CustomerAddressId = NumericString
type CustomerProfileId = NumericString
type CustomerPaymentProfileId = NumericString
type CustomerShippingAddressId = NumericString
type ShippingProfileId = NumericString
type SubscriptionId = NumericString
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

-- | Holds API credentials for Authorize.NET. You should get these when you sign up for a sandbox or production account.
-- | This and 'ApiConfig' are required for every request.
data MerchantAuthentication = MerchantAuthentication {
  merchantAuthentication_name           :: T.Text,
  merchantAuthentication_transactionKey :: T.Text
  } deriving (Eq)

instance Show MerchantAuthentication where
  show x = "MerchantAuthentication { merchantAuthentication_name = \"REDACTED\", merchantAuthentication_transactionKey = \"REDACTED\" }"

-- instance ToJSON MerchantAuthentication where
--   toEncoding = genericToEncoding dropRecordName

-- instance FromJSON MerchantAuthentication where
--   parseJSON value =
--     let objectParse o = MerchantAuthentication <$> o .: "name" <*> o .: "transactionKey"
--     in withObject "MerchantAuthentication" objectParse value

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

-- instance ToJSON NameAndAddress where
--   toEncoding = genericToEncoding dropRecordName

-- instance FromJSON NameAndAddress where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:cardArt
data CardArt = CardArt {
  cardArt_cardBrand       :: Maybe T.Text,
  cardArt_cardImageHeight :: Maybe T.Text,
  cardArt_cardImageUrl    :: Maybe T.Text,
  cardArt_cardImageWidth  :: Maybe T.Text,
  cardArt_cardType        :: Maybe T.Text
  } deriving (Eq, Show)

-- instance ToJSON CardArt where
--   toEncoding = genericToEncoding dropRecordName

-- instance FromJSON CardArt where
--   parseJSON = genericParseJSON dropRecordName
               
data CreditCard = CreditCard {
  -- Extension fields from anet:creditCardSimpleType
  creditCard_cardNumber     :: T.Text,
  creditCard_expirationDate :: T.Text,
  
  creditCard_cardCode       :: Maybe NumericString,
  creditCard_isPaymentToken :: Maybe Bool,
  creditCard_cryptogram     :: Maybe T.Text
  } deriving (Eq, Show)

-- instance ToJSON CreditCard where
--   toEncoding = genericToEncoding dropRecordName

-- instance FromJSON CreditCard where
--   parseJSON = genericParseJSON dropRecordName

mkCreditCard :: T.Text -> T.Text -> Maybe CardCode -> CreditCard
mkCreditCard cardNumber expirationDate cardCode = CreditCard cardNumber expirationDate cardCode Nothing Nothing

data CreditCardMasked = CreditCardMasked {
  creditCardMasked_cardNumber     :: T.Text,
  creditCardMasked_expirationDate :: T.Text,
  creditCardMasked_cardType       :: Maybe T.Text,
  creditCardMasked_cardArt        :: Maybe CardArt
  } deriving (Eq, Show)

-- instance ToJSON CreditCardMasked where
--   toEncoding = genericToEncoding dropRecordName

-- instance FromJSON CreditCardMasked where
--   parseJSON = genericParseJSON dropRecordName


mkCreditCardMasked :: T.Text -> T.Text -> CreditCardMasked
mkCreditCardMasked cardNumber expirationDate = CreditCardMasked cardNumber expirationDate Nothing Nothing

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

-- instance ToJSON CustomerAddress where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON CustomerAddress where
--   parseJSON = genericParseJSON dropRecordName

mkCustomerAddress :: CustomerAddress
mkCustomerAddress = CustomerAddress Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | anet:driversLicenseType
data DriversLicense = DriversLicense {
  driversLicense_number      :: T.Text,
  driversLicense_state       :: T.Text,
  driversLicense_dateOfBirth :: T.Text
  } deriving (Eq, Show)

-- instance ToJSON DriversLicense where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON DriversLicense where
--   parseJSON = genericParseJSON dropRecordName

data BankAccountType = BankAccountType_Individual
                     | BankAccountType_Business
                     deriving (Eq, Show)

-- instance ToJSON BankAccountType where
--   toEncoding = genericToEncoding enumType
-- instance FromJSON BankAccountType where
--   parseJSON = genericParseJSON enumType      
    

-- | anet:echeckTypeEnum
data EcheckType = Echeck_PPD
                | Echeck_WEB
                | Echeck_CCD
                | Echeck_TEL
                | Echeck_ARC
                | Echeck_BOC
                deriving (Eq, Show)

-- instance ToJSON EcheckType where
--   toEncoding = genericToEncoding enumType
-- instance FromJSON EcheckType where
--   parseJSON = genericParseJSON enumType

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

-- instance ToJSON TransactionType where
--   toEncoding = genericToEncoding enumType
-- instance FromJSON TransactionType where
--   parseJSON = genericParseJSON enumType                              

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

-- instance ToJSON BankAccount where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON BankAccount where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:bankAccountMaskedType
data BankAccountMasked = BankAccountMasked {
  bankAccountMasked_accountType   :: Maybe BankAccountType,
  bankAccountMasked_routingNumber :: T.Text,
  bankAccountMasked_accountNumber :: T.Text,
  bankAccountMasked_nameOnAccount :: T.Text,
  bankAccountMasked_echeckType    :: Maybe EcheckType,
  bankAccountMasked_bankName      :: Maybe T.Text
  } deriving (Eq, Show)

-- instance ToJSON BankAccountMasked where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON BankAccountMasked where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:creditCardTrackType
data CreditCardTrack = CreditCardTrack {
  creditCardTrack_track1   :: Maybe T.Text,
  creditCardTrack_track2   :: Maybe T.Text,
  creditCardTrack_cardCode :: Maybe CardCode
  } deriving (Eq, Show)

-- instance ToJSON CreditCardTrack where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON CreditCardTrack where
--   parseJSON = genericParseJSON dropRecordName
                       
-- | anet:encryptedTrackDataType
data EncryptedTrackData = EncryptedTrackData deriving (Eq, Show)
-- instance ToJSON EncryptedTrackData where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON EncryptedTrackData where
--   parseJSON = genericParseJSON dropRecordName
  
-- | anet:payPalType
data PayPal = PayPal deriving (Eq, Show)
-- instance ToJSON PayPal where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON PayPal where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:opaqueDataType
data OpaqueData = OpaqueData deriving (Eq, Show)
-- instance ToJSON OpaqueData where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON OpaqueData where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:paymentEmvType
data PaymentEmv = PaymentEmv deriving (Eq, Show)
-- instance ToJSON PaymentEmv where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON PaymentEmv where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:paymentType
data Payment = Payment_creditCard CreditCard
             | Payment_bankAccount BankAccount
             | Payment_trackData CreditCardTrack
             | Payment_encryptedTrackData EncryptedTrackData
             | Payment_payPal PayPal
             | Payment_opaqueData OpaqueData
             | Payment_emv PaymentEmv
             deriving (Eq, Show)

-- instance ToJSON Payment where
--   toEncoding = genericToEncoding choiceType
-- instance FromJSON Payment where
--   parseJSON = genericParseJSON choiceType

-- | anet:tokenMaskedType
data TokenMasked = TokenMasked {
  tokenMasked_tokenSource    :: Maybe T.Text,
  tokenMasked_tokenNumber    :: T.Text,
  tokenMasked_expirationDate :: T.Text
  } deriving (Eq, Show)

-- instance ToJSON TokenMasked where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON TokenMasked where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:paymentMaskedType

data PaymentMasked = PaymentMasked_creditCard CreditCardMasked
                   | PaymentMasked_bankAccount BankAccountMasked
                   | PaymentMasked_tokenInformation TokenMasked
                   deriving (Eq, Show)

-- instance ToJSON PaymentMasked where
--   toEncoding = genericToEncoding choiceType
-- instance FromJSON PaymentMasked where
--   parseJSON = genericParseJSON choiceType                       
    

-- | anet:paymentProfile
data PaymentProfile = PaymentProfile {
  paymentProfile_paymentProfileId :: NumericString,
  paymentProfile_cardCode         :: Maybe CardCode
  } deriving (Eq, Show)

-- instance ToJSON PaymentProfile where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON PaymentProfile where
--   parseJSON = genericParseJSON dropRecordName
                      
-- | anet:customerPaymentProfileType
data CustomerPaymentProfile = CustomerPaymentProfile {
  customerPaymentProfile_customerType   :: Maybe CustomerType,
  customerPaymentProfile_billTo         :: Maybe CustomerAddress,
  customerPaymentProfile_payment        :: Maybe Payment,
  customerPaymentProfile_driversLicense :: Maybe DriversLicense,
  customerPaymentProfile_taxId          :: Maybe TaxId
  } deriving (Eq, Show)

-- instance ToJSON CustomerPaymentProfile where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON CustomerPaymentProfile where
--   parseJSON = genericParseJSON dropRecordName

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

-- instance ToJSON CustomerPaymentProfileEx where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON CustomerPaymentProfileEx where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:customerPaymentProfileMaskedType
data CustomerPaymentProfileMasked = CustomerPaymentProfileMasked {
  customerPaymentProfileMasked_customerProfileId        :: Maybe CustomerProfileId,
  customerPaymentProfileMasked_customerPaymentProfileId :: CustomerPaymentProfileId,
  customerPaymentProfileMasked_payment                  :: Maybe PaymentMasked,
  customerPaymentProfileMasked_driversLicense           :: Maybe DriversLicense,
  customerPaymentProfileMasked_taxId                    :: Maybe TaxId,
  customerPaymentProfileMasked_subscriptionIds          :: Maybe [SubscriptionId]
  } deriving (Eq, Show)
                                    
-- instance ToJSON CustomerPaymentProfileMasked where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON CustomerPaymentProfileMasked where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:CustomerPaymentProfileSearchTypeEnum
data CustomerPaymentProfileSearchType = SearchType_cardsExpiringInMonth deriving (Eq, Show)

-- instance ToJSON CustomerPaymentProfileSearchType where
--   toEncoding = genericToEncoding enumType
-- instance FromJSON CustomerPaymentProfileSearchType where
--   parseJSON = genericParseJSON enumType

data CustomerPaymentProfileOrderFieldEnum = OrderField_id deriving (Eq, Show)

-- instance ToJSON CustomerPaymentProfileOrderFieldEnum where
--   toEncoding = genericToEncoding enumType
-- instance FromJSON CustomerPaymentProfileOrderFieldEnum where
--   parseJSON = genericParseJSON enumType

-- | anet:CustomerPaymentProfileSorting
data CustomerPaymentProfileSorting = CustomerPaymentProfileSorting {
  customerPaymentProfileSorting_orderBy         :: CustomerPaymentProfileOrderFieldEnum,
  customerPaymentProfileSorting_orderDescending :: Bool
  } deriving (Eq, Show)

-- instance ToJSON CustomerPaymentProfileSorting where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON CustomerPaymentProfileSorting where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:Paging
data Paging = Paging {
  paging_limit :: NumericString,
  paging_offset :: NumericString
  } deriving (Eq, Show)

-- instance ToJSON Paging where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON Paging where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:customerPaymentProfileListItemType
data CustomerPaymentProfileListItem = CustomerPaymentProfileListItem {
  customerPaymentProfileListItem_customerPaymentProfileId :: CustomerPaymentProfileId,
  customerPaymentProfileListItem_customerProfileId        :: CustomerProfileId,
  customerPaymentProfileListItem_billTo                   :: CustomerAddress,
  customerPaymentProfileListItem_payment                  :: PaymentMasked
  } deriving (Eq, Show)
                                      
-- instance ToJSON CustomerPaymentProfileListItem where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON CustomerPaymentProfileListItem where
--   parseJSON = genericParseJSON dropRecordName         
    

-- | anet:arrayOfCustomerPaymentProfileListItemType
data ArrayOfCustomerPaymentProfileListItem = ArrayOfCustomerPaymentProfileListItem {
  arrayOfCustomerPaymentProfileListIitem_paymentProfile :: ArrayOf CustomerPaymentProfileListItem
  } deriving (Eq, Show)

-- instance ToJSON ArrayOfCustomerPaymentProfileListItem where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON ArrayOfCustomerPaymentProfileListItem where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:customerProfileBaseType
data CustomerProfileBase = CustomerProfileBase {
  customerProfileBase_merchantCustomerId :: Maybe T.Text,
  customerProfileBase_description        :: Maybe T.Text,
  customerProfileBase_email              :: Maybe T.Text
  } deriving (Eq, Show)

-- instance ToJSON CustomerProfileBase where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON CustomerProfileBase where
--   parseJSON = genericParseJSON dropRecordName                          
  
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

-- instance ToJSON CustomerProfile where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON CustomerProfile where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:customerAddressExType
data CustomerAddressEx = CustomerAddressEx {
  customerAddressEx_firstName :: Maybe T.Text,
  customerAddressEx_lastName  :: Maybe T.Text,
  customerAddressEx_company   :: Maybe T.Text,
  customerAddressEx_address   :: Maybe T.Text,
  customerAddressEx_city      :: Maybe T.Text,
  customerAddressEx_state     :: Maybe T.Text,
  customerAddressEx_zip       :: Maybe T.Text,
  customerAddressEx_country   :: Maybe T.Text,
  
  customerAddressEx_phoneNumber :: Maybe T.Text,
  customerAddressEx_faxNumber   :: Maybe T.Text,
  customerAddressEx_email       :: Maybe T.Text,
  
  customerAddressEx_customerAddressId :: Maybe CustomerAddressId
  } deriving (Eq, Show)

-- instance ToJSON CustomerAddressEx where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON CustomerAddressEx where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:customerProfileMaskedType
data CustomerProfileMasked = CustomerProfileMasked {
  customerProfileMasked_merchantCustomerId :: Maybe T.Text,
  customerProfileMasked_description        :: Maybe T.Text,
  customerProfileMasked_email              :: Maybe T.Text,
  
  customerProfileMasked_customerProfileId  :: Maybe NumericString,
  
  customerProfileMasked_paymentProfiles :: [CustomerPaymentProfileMasked],
  customerProfileMasked_shipToList      :: Maybe CustomerAddressEx
  } deriving (Eq, Show)

-- instance ToJSON CustomerProfileMasked where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON CustomerProfileMasked where
--   parseJSON = genericParseJSON dropRecordName

data ValidationMode = Validation_none
                    | Validation_testMode
                    | Validation_liveMode
                    -- | Per Authorize.NET: "NOT RECOMMENDED. Use of this option can result in fines from your processor."
                    | Validation_oldLiveMode
                    deriving (Eq, Show)

-- instance ToJSON ValidationMode where
--   toEncoding = genericToEncoding enumType
-- instance FromJSON ValidationMode where
--   parseJSON = genericParseJSON enumType

-- | anet:customerProfilePaymentType
data CustomerProfilePayment = CustomerProfilePayment {
  customerProfilePayment_createProfile     :: Maybe Bool,
  customerProfilePayment_customerProfileId :: Maybe CustomerProfileId,
  customerProfilePayment_paymentProfile    :: Maybe PaymentProfile,
  customerProfilePayment_shippingProfileId :: Maybe ShippingProfileId
  } deriving (Eq, Show)

-- instance ToJSON CustomerProfilePayment where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON CustomerProfilePayment where
--   parseJSON = genericParseJSON dropRecordName

mkCustomerProfilePayment :: CustomerProfilePayment
mkCustomerProfilePayment = CustomerProfilePayment Nothing Nothing Nothing Nothing

-- | anet:solutionType
data Solution = Solution {
  solution_id         :: T.Text,
  solution_name       :: Maybe T.Text,
  solution_vendorName :: Maybe T.Text
  } deriving (Eq, Show)

-- instance ToJSON Solution where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON Solution where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:orderType
data Order = Order {
  order_invoiceNumber :: Maybe T.Text,
  order_description   :: Maybe T.Text
  } deriving (Eq, Show)

-- instance ToJSON Order where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON Order where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:lineItemType
data LineItem = LineItem {
  lineItem_itemId      :: T.Text,
  lineItem_name        :: T.Text,
  lineItem_description :: Maybe T.Text,
  lineItem_quantity    :: Decimal,
  lineItem_unitPrice   :: Decimal,
  lineItem_taxable     :: Maybe Bool
  } deriving (Eq, Show)

-- instance ToJSON LineItem where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON LineItem where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:ArrayOfLineItem
data LineItems = LineItems {
  lineItems_lineItem :: ArrayOf LineItem
  } deriving (Eq, Show)

-- instance ToJSON LineItems where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON LineItems where
--   parseJSON = genericParseJSON dropRecordName                 

-- | anet:extendedAmountType
data ExtendedAmount = ExtendedAmount {
  extendedAmount_amount      :: Decimal,
  extendedAmount_name        :: Maybe T.Text,
  extendedAmount_description :: Maybe T.Text
  } deriving (Eq, Show)

-- instance ToJSON ExtendedAmount where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON ExtendedAmount where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:customerDataType
data CustomerData = CustomerData {
  customerData_type            :: Maybe CustomerType,
  customerData_id              :: Maybe T.Text,
  customerData_email           :: Maybe T.Text,
  customerData_driverseLicense :: Maybe DriversLicense,
  customerData_taxId           :: Maybe TaxId
  } deriving (Eq, Show)

-- instance ToJSON CustomerData where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON CustomerData where
--   parseJSON = genericParseJSON dropRecordName

mkCustomerData :: CustomerData
mkCustomerData = CustomerData Nothing Nothing Nothing Nothing Nothing

-- | anet:ccAuthenticationType
data CcAuthentication = CcAuthentication {
  ccAuthentication_authenticationIndicator       :: T.Text,
  ccAuthentication_cardholderAuthenticationValue :: T.Text
  } deriving (Eq, Show)

-- instance ToJSON CcAuthentication where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON CcAuthentication where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:transRetailInfoType
data TransRetailInfo = TransRetailInfo {
  transRetailInfo_marketType        :: Maybe T.Text,
  transRetailInfo_deviceType        :: Maybe T.Text,
  transRetailInfo_customerSignature :: Maybe T.Text
  } deriving (Eq, Show)

-- instance ToJSON TransRetailInfo where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON TransRetailInfo where
--   parseJSON = genericParseJSON dropRecordName

data SettingName = SettingName_emailCustomer
                 -- | Sends an email to the merchant email address on your Authorize.NET after every purchase
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

-- instance ToJSON SettingName where
--   toEncoding = genericToEncoding enumType
-- instance FromJSON SettingName where
--   parseJSON = genericParseJSON enumType


-- | anet:settingType
data Setting = Setting {
  setting_settingName  :: SettingName,
  setting_settingValue :: T.Text
  } deriving (Eq, Show)

$(deriveXml dropRecordName ''Setting)

-- instance ToJSON Setting where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON Setting where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:userField
data UserField = UserField {
  userField_name  :: T.Text,
  userField_value :: T.Text
  } deriving (Eq, Show)

-- instance ToJSON UserField where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON UserField where
--   parseJSON = genericParseJSON dropRecordName

data SecureAcceptance = SecureAcceptance {
  secureAcceptance_SecureAcceptanceUrl :: T.Text,
  secureAcceptance_PayerID             :: T.Text
  } deriving (Eq, Show)

-- instance ToJSON SecureAcceptance where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON SecureAcceptance where
--   parseJSON = genericParseJSON dropRecordName

data EmvResponse = EmvResponse {
  emvResponse_tsvData :: Maybe T.Text,
  emvResponse_tag     :: Maybe T.Text
  } deriving (Eq, Show)

-- instance ToJSON EmvResponse where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON EmvResponse where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:transactionRequestType
data TransactionRequest = TransactionRequest {
  transactionRequest_transactionType          :: TransactionType,
  -- | Total amount(including taxes, duty, shipping, etc.) to charge the card. A decimal number like "8.45".
  transactionRequest_amount                   :: Decimal,
  -- | Currency code. A common one is "USD".
  transactionRequest_currencyCode             :: Maybe T.Text,
  transactionRequest_payment                  :: Maybe Payment,
  transactionRequest_profile                  :: Maybe CustomerProfilePayment,
  transactionRequest_solution                 :: Maybe Solution,
  transactionRequest_callId                   :: Maybe T.Text,
  -- | An identification number assigned to each POS (Point of Sale) device by a merchant's processor. This number allows the processor to identify the source of a transaction.
  transactionRequest_terminalNumber           :: Maybe T.Text,
  -- | Authorization code. This may have been obtained from a verbal authorization or through another channel.
  transactionRequest_authCode                 :: Maybe T.Text,
  -- | Transaction ID of the original partial authorization transaction. 
  -- | Required only for refundTransaction, priorAuthCaptureTransaction, and voidTransaction. Do not include this field if you are providing splitTenderId
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

-- instance ToJSON TransactionRequest where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON TransactionRequest where
--   parseJSON = genericParseJSON dropRecordName

-- | The TransactionRequest type has a lot of Maybe fields, so use this to get a bare-bones default.
mkTransactionRequest :: TransactionType -> Decimal -> TransactionRequest
mkTransactionRequest transactionType amount = TransactionRequest transactionType amount Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | anet:messageTypeEnum
data MessageType = Message_Ok
                 | Message_Error
                 deriving (Eq, Show)
-- instance ToJSON MessageType where
--   toEncoding = genericToEncoding enumType
-- instance FromJSON MessageType where
--   parseJSON = genericParseJSON enumType         
    

-- | The possible message codes are documented at http://developer.authorize.net/api/reference/dist/json/responseCodes.json
data Message = Message {
  message_code :: T.Text,
  message_text :: T.Text
  } deriving (Eq, Show)

-- instance ToJSON Message where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON Message where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:messagesType
data Messages = Messages {
  messages_resultCode :: MessageType,
  messages_message    :: ArrayOf Message
  } deriving (Eq, Show)

-- instance ToJSON Messages where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON Messages where
--   parseJSON = genericParseJSON dropRecordName

-- | anet:transactionResponse has an element called 'prePaidCard' with an anonymous type
data PrePaidCard = PrePaidCard {
  prePaidCard_requestedAmount :: Maybe T.Text,
  prePaidCard_approvedAmount  :: Maybe T.Text,
  prePaidCard_balanceOnCard   :: Maybe T.Text
  } deriving (Eq, Show)

-- instance ToJSON PrePaidCard where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON PrePaidCard where
--   parseJSON = genericParseJSON dropRecordName

data TransactionResponse_message = TransactionResponse_message {
  transactionResponseMessage_code        :: Maybe T.Text,
  transactionResponseMessage_description :: Maybe T.Text
  } deriving (Eq, Show)

-- instance ToJSON TransactionResponse_message where
--   toEncoding = genericToEncoding choiceType
-- instance FromJSON TransactionResponse_message where
--   parseJSON = genericParseJSON choiceType

data TransactionResponse_error = TransactionResponse_error {
  transactionResponseError_errorCode :: Maybe T.Text,
  transactionResponseError_errorText :: Maybe T.Text
  } deriving (Eq, Show)

-- instance ToJSON TransactionResponse_error where
--   toEncoding = genericToEncoding choiceType
-- instance FromJSON TransactionResponse_error where
--   parseJSON = genericParseJSON choiceType

data TransactionResponse_splitTenderPayment = TransactionResponse_splitTenderPayment {
  transactionResponseSplitTenderPayment_transId            :: Maybe T.Text,
  transactionResponseSplitTenderPayment_responseCode       :: Maybe T.Text,
  transactionResponseSplitTenderPayment_responseToCustomer :: Maybe T.Text,
  transactionResponseSplitTenderPayment_authCode           :: Maybe T.Text,
  transactionResponseSplitTenderPayment_accountNumber      :: Maybe T.Text,
  transactionResponseSplitTenderPayment_accountType        :: Maybe T.Text,
  transactionResponseSplitTenderPayment_requestedAmount    :: Maybe T.Text,
  transactionResponseSplitTenderPayment_approvedAmount     :: Maybe T.Text,
  transactionResponseSplitTenderPayment_balanceOnCard      :: Maybe T.Text              
  } | D3_DummyForAeson deriving (Eq, Show)

-- instance ToJSON TransactionResponse_splitTenderPayment where
--   toEncoding = genericToEncoding choiceType
-- instance FromJSON TransactionResponse_splitTenderPayment where
--   parseJSON = genericParseJSON choiceType

-- anet:ArrayOfSetting
data ArrayOfSetting = ArrayOfSetting {
  arrayOfSetting_setting :: ArrayOf Setting
  } deriving (Eq, Show)

$(deriveXml dropRecordName ''ArrayOfSetting)

-- instance ToJSON ArrayOfSetting where
--   toEncoding = genericToEncoding dropRecordName
-- instance FromJSON ArrayOfSetting where
--   parseJSON = genericParseJSON dropRecordName
