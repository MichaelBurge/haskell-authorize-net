{-# LANGUAGE EmptyDataDecls, GADTs, StandaloneDeriving, OverloadedStrings, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveFoldable, OverloadedLists #-}

-- | Implements the Authorize.NET JSON API. Types generally correspond to those defined in the XSD.
-- | XSD location: https://api.authorize.net/xml/v1/schema/AnetApiSchema.xsd
module Network.AuthorizeNet.Types (
  module Network.AuthorizeNet.Types
  ) where

import Control.Applicative
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
-- import Data.Aeson
-- import Data.Aeson.TH
-- import Data.Aeson.Types hiding (Parser)
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.String
import GHC.Exts
import Network.Wreq hiding (Proxy)
import Text.XML.HaXml.Schema.Schema (SchemaType(..), SimpleType(..), Extension(..), Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

-- | These let you control exactly what shows in the XML header information
data XmlNamespaceLevel = Namespace_none
                       | Namespace_xsd
                       | Namespace_full
                       deriving (Eq, Show)

class SchemaType a => XmlParsable a where
  xmlParsableName :: a -> String
  xmlNamespaceLevel :: a -> XmlNamespaceLevel
  

class XmlParsable a => ApiRequest a where
  type ResponseType a

class XmlParsable a => ApiResponse a where
  aNetApiResponse :: a -> ANetApiResponse

-- | There are 5 types of hosted forms: http://developer.authorize.net/api/reference/features/customer_profiles.html
data CimHostedProfileForm = CimHosted_Manage
                          | CimHosted_AddPayment
                          | CimHosted_EditPayment
                          | CimHosted_AddShipping
                          | CimHosted_EditShipping
                          deriving (Eq, Show)

-- | Information about the Authorize.NET API's endpoint. See 'API Endpoints' at http://developer.authorize.net/api/reference/index.html
-- | If you had a mock of their API set up somewhere for unit tests, you would use it by creating a value of this type.
-- | This and 'MerchantAuthentication' are required for every request
data ApiConfig = ApiConfig {
  apiConfig_baseUrl              :: String,
  apiConfig_hostedProfileUrlBase :: T.Text,
  apiConfig_simPostUrl           :: T.Text
  } deriving (Show)

newtype NumericString = NumericString { unNumericString :: Integer } deriving (Eq, Ord, Show, Num)

--newtype Decimal = Decimal T.Text deriving (Eq, Show, IsString, ToJSON, FromJSON)
newtype Decimal = Decimal T.Text deriving (Eq, Show, IsString)

-- | Creates a Decimal from a number of USD cents.
mkDecimal :: Int -> Decimal
mkDecimal priceCents =
  let dollars = priceCents `div` 100
      cents   = priceCents `mod` 100
  in Decimal $ T.pack $ show dollars ++ "." ++ show cents

-- | Some Authorize.NET services in their JSON represent a single element as a single-element list, and others use an object. This type normalizes them into a list.
data ArrayOf a = ArrayOf [a] deriving (Eq, Show, Foldable)

type CustomerAddressId = NumericString
type CustomerProfileId = NumericString
type CustomerPaymentProfileId = NumericString
type CustomerShippingAddressId = NumericString
type ShippingProfileId = NumericString
type SubscriptionId = NumericString
type TransactionId = NumericString
type TaxId = T.Text
type MerchantCustomerId = NumericString
type AuthCode = T.Text
type AvsCode = T.Text
type InvoiceNumber = Int

data ArrayOfString = ArrayOfString {
  arrayOfString_string :: ArrayOf T.Text
  } deriving (Eq, Show)

data ArrayOfNumericString = ArrayOfNumericString {
  arrayOfNumericString_numericString :: ArrayOf NumericString
  } deriving (Eq, Show)

data SubscriptionIdList = SubscriptionIdList {
  subscriptionIdList_subscriptionId :: ArrayOf NumericString
  } deriving (Eq, Show)

type CardCode = NumericString
-- | Holds API credentials for Authorize.NET. You should get these when you sign up for a sandbox or production account.
-- | This and 'ApiConfig' are required for every request.
data MerchantAuthentication = MerchantAuthentication {
  merchantAuthentication_name           :: T.Text,
  merchantAuthentication_transactionKey :: T.Text
  } deriving (Eq)

instance Show MerchantAuthentication where
  show x = "MerchantAuthentication { merchantAuthentication_name = \"REDACTED\", merchantAuthentication_transactionKey = \"REDACTED\" }"

-- | anet:customerTypeEnum
data CustomerType = CustomerType_individual
                  | CustomerType_business
                  deriving (Eq, Show)

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

-- | anet:cardArt
data CardArt = CardArt {
  cardArt_cardBrand       :: Maybe T.Text,
  cardArt_cardImageHeight :: Maybe T.Text,
  cardArt_cardImageUrl    :: Maybe T.Text,
  cardArt_cardImageWidth  :: Maybe T.Text,
  cardArt_cardType        :: Maybe T.Text
  } deriving (Eq, Show)

data CreditCard = CreditCard {
  -- Extension fields from anet:creditCardSimpleType
  creditCard_cardNumber     :: T.Text,
  creditCard_expirationDate :: T.Text,
  
  creditCard_cardCode       :: Maybe NumericString,
  creditCard_isPaymentToken :: Maybe Bool,
  creditCard_cryptogram     :: Maybe T.Text
  } deriving (Eq, Show)

mkCreditCard :: T.Text -> T.Text -> Maybe CardCode -> CreditCard
mkCreditCard cardNumber expirationDate cardCode = CreditCard cardNumber expirationDate cardCode Nothing Nothing

data CreditCardMasked = CreditCardMasked {
  creditCardMasked_cardNumber     :: T.Text,
  creditCardMasked_expirationDate :: T.Text,
  creditCardMasked_cardType       :: Maybe T.Text,
  creditCardMasked_cardArt        :: Maybe CardArt
  } deriving (Eq, Show)

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

mkCustomerAddress :: CustomerAddress
mkCustomerAddress = CustomerAddress Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | anet:driversLicenseType
data DriversLicense = DriversLicense {
  driversLicense_number      :: T.Text,
  driversLicense_state       :: T.Text,
  driversLicense_dateOfBirth :: T.Text
  } deriving (Eq, Show)

data BankAccountType = BankAccountType_checking
                     | BankAccountType_savings
                     | BankAccountType_businessChecking
                     deriving (Eq, Show)

-- | anet:echeckTypeEnum
data EcheckType = Echeck_PPD
                | Echeck_WEB
                | Echeck_CCD
                | Echeck_TEL
                | Echeck_ARC
                | Echeck_BOC
                deriving (Eq, Show)

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

-- | anet:bankAccountMaskedType
data BankAccountMasked = BankAccountMasked {
  bankAccountMasked_accountType   :: Maybe BankAccountType,
  bankAccountMasked_routingNumber :: T.Text,
  bankAccountMasked_accountNumber :: T.Text,
  bankAccountMasked_nameOnAccount :: T.Text,
  bankAccountMasked_echeckType    :: Maybe EcheckType,
  bankAccountMasked_bankName      :: Maybe T.Text
  } deriving (Eq, Show)

-- | anet:creditCardTrackType
data CreditCardTrack = CreditCardTrack {
  creditCardTrack_track1   :: Maybe T.Text,
  creditCardTrack_track2   :: Maybe T.Text,
  creditCardTrack_cardCode :: Maybe CardCode
  } deriving (Eq, Show)
                       
-- | anet:encryptedTrackDataType
data EncryptedTrackData = EncryptedTrackData_dummy deriving (Eq, Show)

-- | anet:payPalType
data PayPal = PayPal_dummy deriving (Eq, Show)

-- | anet:opaqueDataType
data OpaqueData = OpaqueData_dummy  deriving (Eq, Show)

-- | anet:paymentEmvType
data PaymentEmv = PaymentEmv_dummy deriving (Eq, Show)

-- | anet:paymentType
data Payment = Payment_creditCard CreditCard
             | Payment_bankAccount BankAccount
             | Payment_trackData CreditCardTrack
             | Payment_encryptedTrackData EncryptedTrackData
             | Payment_payPal PayPal
             | Payment_opaqueData OpaqueData
             | Payment_emv PaymentEmv
             deriving (Eq, Show)

-- | anet:tokenMaskedType
data TokenMasked = TokenMasked {
  tokenMasked_tokenSource    :: Maybe T.Text,
  tokenMasked_tokenNumber    :: T.Text,
  tokenMasked_expirationDate :: T.Text
  } deriving (Eq, Show)

-- | anet:paymentMaskedType
data PaymentMasked = PaymentMasked_creditCard CreditCardMasked
                   | PaymentMasked_bankAccount BankAccountMasked
                   | PaymentMasked_tokenInformation TokenMasked
                   deriving (Eq, Show)

-- | anet:paymentProfile
data PaymentProfile = PaymentProfile {
  paymentProfile_paymentProfileId :: NumericString,
  paymentProfile_cardCode         :: Maybe CardCode
  } deriving (Eq, Show)
                      
-- | anet:customerPaymentProfileType
data CustomerPaymentProfile = CustomerPaymentProfile {
  customerPaymentProfile_customerType   :: Maybe CustomerType,
  customerPaymentProfile_billTo         :: Maybe CustomerAddress,
  customerPaymentProfile_payment        :: Maybe Payment,
  customerPaymentProfile_driversLicense :: Maybe DriversLicense,
  customerPaymentProfile_taxId          :: Maybe TaxId
  } deriving (Eq, Show)

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

-- | anet:customerPaymentProfileMaskedType
data CustomerPaymentProfileMasked = CustomerPaymentProfileMasked {
  customerPaymentProfileMasked_customerType             :: Maybe CustomerType,
  customerPaymentProfileMasked_billTo                   :: Maybe CustomerAddress,
  
  customerPaymentProfileMasked_customerProfileId        :: Maybe CustomerProfileId,
  customerPaymentProfileMasked_customerPaymentProfileId :: CustomerPaymentProfileId,
  customerPaymentProfileMasked_payment                  :: Maybe PaymentMasked,
  customerPaymentProfileMasked_driversLicense           :: Maybe DriversLicense,
  customerPaymentProfileMasked_taxId                    :: Maybe TaxId,
  customerPaymentProfileMasked_subscriptionIds          :: Maybe SubscriptionIdList
  } deriving (Eq, Show)

mkCustomerPaymentProfileMasked :: CustomerPaymentProfileId -> CustomerPaymentProfileMasked
mkCustomerPaymentProfileMasked customerPaymentProfileId = CustomerPaymentProfileMasked Nothing Nothing Nothing customerPaymentProfileId Nothing Nothing Nothing Nothing

-- | anet:CustomerPaymentProfileSearchTypeEnum
data CustomerPaymentProfileSearchType = SearchType_cardsExpiringInMonth deriving (Eq, Show)

data CustomerPaymentProfileOrderFieldEnum = OrderField_id deriving (Eq, Show)

-- | anet:CustomerPaymentProfileSorting
data CustomerPaymentProfileSorting = CustomerPaymentProfileSorting {
  customerPaymentProfileSorting_orderBy         :: CustomerPaymentProfileOrderFieldEnum,
  customerPaymentProfileSorting_orderDescending :: Bool
  } deriving (Eq, Show)

-- | anet:Paging
data Paging = Paging {
  paging_limit :: NumericString,
  paging_offset :: NumericString
  } deriving (Eq, Show)

-- | anet:customerPaymentProfileListItemType
data CustomerPaymentProfileListItem = CustomerPaymentProfileListItem {
  customerPaymentProfileListItem_customerPaymentProfileId :: CustomerPaymentProfileId,
  customerPaymentProfileListItem_customerProfileId        :: CustomerProfileId,
  customerPaymentProfileListItem_billTo                   :: CustomerAddress,
  customerPaymentProfileListItem_payment                  :: PaymentMasked
  } deriving (Eq, Show)

-- | anet:arrayOfCustomerPaymentProfileListItemType
data ArrayOfCustomerPaymentProfileListItem = ArrayOfCustomerPaymentProfileListItem {
  arrayOfCustomerPaymentProfileListIitem_paymentProfile :: ArrayOf CustomerPaymentProfileListItem
  } deriving (Eq, Show)

-- | anet:customerProfileBaseType
data CustomerProfileBase = CustomerProfileBase {
  customerProfileBase_merchantCustomerId :: Maybe T.Text,
  customerProfileBase_description        :: Maybe T.Text,
  customerProfileBase_email              :: Maybe T.Text
  } deriving (Eq, Show)

-- | anet:customerProfileType
-- | Contains a 'Maybe' 'PaymentProfile' and 'Maybe' 'CustomerAddress' instead of an unbounded list of these due to JSON not supporting duplicate keys.
data CustomerProfile = CustomerProfile {
  customerProfile_merchantCustomerId :: T.Text,
  customerProfile_description        :: T.Text,
  customerProfile_email              :: T.Text,
  customerProfile_paymentProfiles    :: Maybe CustomerPaymentProfile,
  customerProfile_shipTos            :: Maybe CustomerAddress
  } deriving (Eq, Show)

-- | anet:customerProfileExType
data CustomerProfileEx = CustomerProfileEx {
  customerProfileEx_merchantCustomerId :: T.Text,
  customerProfileEx_description        :: T.Text,
  customerProfileEx_email              :: T.Text,
  customerProfileEx_customerProfileId  :: Maybe CustomerProfileId
  } deriving (Eq, Show)

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

-- | anet:customerProfileMaskedType
data CustomerProfileMasked = CustomerProfileMasked {
  customerProfileMasked_merchantCustomerId :: Maybe T.Text,
  customerProfileMasked_description        :: Maybe T.Text,
  customerProfileMasked_email              :: Maybe T.Text,
  
  customerProfileMasked_customerProfileId  :: Maybe NumericString,
  
  customerProfileMasked_paymentProfiles :: ArrayOf CustomerPaymentProfileMasked,
  customerProfileMasked_shipToList      :: ArrayOf CustomerAddressEx
  } deriving (Eq, Show)

data ValidationMode = Validation_none
                    | Validation_testMode
                    | Validation_liveMode
                    -- | Per Authorize.NET: "NOT RECOMMENDED. Use of this option can result in fines from your processor."
                    | Validation_oldLiveMode
                    deriving (Eq, Show)

-- | anet:customerProfilePaymentType
data CustomerProfilePayment = CustomerProfilePayment {
  customerProfilePayment_createProfile     :: Maybe Bool,
  customerProfilePayment_customerProfileId :: Maybe CustomerProfileId,
  customerProfilePayment_paymentProfile    :: Maybe PaymentProfile,
  customerProfilePayment_shippingProfileId :: Maybe ShippingProfileId
  } deriving (Eq, Show)

mkCustomerProfilePayment :: CustomerProfilePayment
mkCustomerProfilePayment = CustomerProfilePayment Nothing Nothing Nothing Nothing

-- | anet:solutionType
data Solution = Solution {
  solution_id         :: T.Text,
  solution_name       :: Maybe T.Text,
  solution_vendorName :: Maybe T.Text
  } deriving (Eq, Show)

-- | anet:orderType
data Order = Order {
  order_invoiceNumber :: Maybe T.Text,
  order_description   :: Maybe T.Text
  } deriving (Eq, Show)

-- | anet:lineItemType
data LineItem = LineItem {
  lineItem_itemId      :: T.Text,
  lineItem_name        :: T.Text,
  lineItem_description :: Maybe T.Text,
  lineItem_quantity    :: Decimal,
  lineItem_unitPrice   :: Decimal,
  lineItem_taxable     :: Maybe Bool
  } deriving (Eq, Show)

-- | anet:ArrayOfLineItem
data LineItems = LineItems {
  lineItems_lineItem :: ArrayOf LineItem
  } deriving (Eq, Show)

-- | anet:extendedAmountType
data ExtendedAmount = ExtendedAmount {
  extendedAmount_amount      :: Decimal,
  extendedAmount_name        :: Maybe T.Text,
  extendedAmount_description :: Maybe T.Text
  } deriving (Eq, Show)

-- | anet:customerDataType
data CustomerData = CustomerData {
  customerData_type            :: Maybe CustomerType,
  customerData_id              :: Maybe T.Text,
  customerData_email           :: Maybe T.Text,
  customerData_driverseLicense :: Maybe DriversLicense,
  customerData_taxId           :: Maybe TaxId
  } deriving (Eq, Show)

mkCustomerData :: CustomerData
mkCustomerData = CustomerData Nothing Nothing Nothing Nothing Nothing

-- | anet:ccAuthenticationType
data CcAuthentication = CcAuthentication {
  ccAuthentication_authenticationIndicator       :: T.Text,
  ccAuthentication_cardholderAuthenticationValue :: T.Text
  } deriving (Eq, Show)

-- | anet:transRetailInfoType
data TransRetailInfo = TransRetailInfo {
  transRetailInfo_marketType        :: Maybe T.Text,
  transRetailInfo_deviceType        :: Maybe T.Text,
  transRetailInfo_customerSignature :: Maybe T.Text
  } deriving (Eq, Show)

data SettingName = SettingName_emailCustomer
                 -- | Sends an email to the merchant email address on your Authorize.NET after every purchase
                 | SettingName_merchantEmail
                 | SettingName_allowPartialAuth
                 | SettingName_headerEmailReceipt
                 | SettingName_footerEmailReceipt
                 | SettingName_recurringBilling
                 | SettingName_duplicateWindow
                 | SettingName_testRequest
                 | SettingName_hostedProfileReturnUrlText
                 | SettingName_hostedProfileReturnUrl
                 | SettingName_hostedProfilePageBorderVisible
                 | SettingName_hostedProfileIFrameCommunicatorUrl
                 | SettingName_hostedProfileHeadingBgColor
                 | SettingName_hostedProfileValidationMode
                 | SettingName_hostedProfileBillingAddressRequired
                 | SettingName_hostedProfileCardCodeRequired
                 deriving (Eq, Show)

-- | anet:settingType
data Setting = Setting {
  setting_settingName  :: SettingName,
  setting_settingValue :: T.Text
  } deriving (Eq, Show)

-- anet:ArrayOfSetting
data ArrayOfSetting = ArrayOfSetting {
  arrayOfSetting_setting :: ArrayOf Setting
  } deriving (Eq, Show)

-- | anet:userField
data UserField = UserField {
  userField_name  :: T.Text,
  userField_value :: T.Text
  } deriving (Eq, Show)

data ArrayOfUserField = ArrayOfUserField {
  arrayOfUserField_userField :: ArrayOf UserField
  } deriving (Eq, Show)

data SecureAcceptance = SecureAcceptance {
  secureAcceptance_SecureAcceptanceUrl :: T.Text,
  secureAcceptance_PayerID             :: T.Text
  } deriving (Eq, Show)

data EmvResponse = EmvResponse {
  emvResponse_tsvData :: Maybe T.Text,
  emvResponse_tag     :: Maybe T.Text
  } deriving (Eq, Show)

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
  transactionRequest_transactionSettings      :: Maybe ArrayOfSetting,
  transactionRequest_userFields               :: Maybe UserField
  } deriving (Eq, Show)

-- | The TransactionRequest type has a lot of Maybe fields, so use this to get a bare-bones default.
mkTransactionRequest :: TransactionType -> Decimal -> TransactionRequest
mkTransactionRequest transactionType amount = TransactionRequest transactionType amount Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | anet:messageTypeEnum
data MessageType = Message_Ok
                 | Message_Error
                 deriving (Eq, Show)

-- | The possible message codes are documented at http://developer.authorize.net/api/reference/dist/json/responseCodes.json
data Message = Message {
  message_code :: T.Text,
  message_text :: T.Text
  } deriving (Eq, Show)

-- | anet:messagesType
data Messages = Messages {
  messages_resultCode :: MessageType,
  messages_message    :: ArrayOf Message
  } deriving (Eq, Show)

-- | anet:transactionResponse has an element called 'prePaidCard' with an anonymous type
data PrePaidCard = PrePaidCard {
  prePaidCard_requestedAmount :: Maybe T.Text,
  prePaidCard_approvedAmount  :: Maybe T.Text,
  prePaidCard_balanceOnCard   :: Maybe T.Text
  } deriving (Eq, Show)
                   
data TransactionResponse_message = TransactionResponse_message {
  transactionResponseMessage_code        :: Maybe T.Text,
  transactionResponseMessage_description :: Maybe T.Text
  } deriving (Eq, Show)

data ArrayOfTransactionResponseMessage = ArrayOfTransactionResponseMessage {
  arrayOfTransactionResponseMessage_message :: ArrayOf TransactionResponse_message
  } deriving (Eq, Show)

data TransactionResponse_error = TransactionResponse_error {
  transactionResponseError_errorCode :: Maybe T.Text,
  transactionResponseError_errorText :: Maybe T.Text
  } deriving (Eq, Show)

data ArrayOfTransactionResponseError = ArrayOfTransactionResponseError {
  arrayOfTransactionResponseMessage_error :: ArrayOf TransactionResponse_error
  } deriving (Eq, Show)

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
  } deriving (Eq, Show)

data ArrayOfTransactionResponseSplitTenderPayment = ArrayOfTransactionResponseSplitTenderPayment {
  arrayOfTransactionResponseSplitTenderPayment_splitTenderPayment :: ArrayOf TransactionResponse_splitTenderPayment
  } deriving (Eq, Show)

-- | anet:ANetApiResponse
data ANetApiResponse = ANetApiResponse {
  aNetApiResponse_refId        :: Maybe T.Text,
  aNetApiResponse_messages     :: Messages,
  aNetApiResponse_sessionToken :: Maybe T.Text
  } deriving (Eq, Show)

