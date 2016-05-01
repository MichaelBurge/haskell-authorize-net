{-# LANGUAGE TypeFamilies, TemplateHaskell, MultiParamTypeClasses, OverloadedLists,RecordWildCards #-}

-- | Anything that isn't a standard class like Eq, Show, or Ord goes in this module.
module Network.AuthorizeNet.Instances where

import Network.AuthorizeNet.Request
import Network.AuthorizeNet.Response
import Network.AuthorizeNet.TH
import Network.AuthorizeNet.Types

import Language.Haskell.TH.Lift

import GHC.Exts
import Text.XML.HaXml
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as XML

import qualified Data.Text as T

import qualified Text.Parse as Parse

instance SchemaType T.Text where
  parseSchemaType s = do
    e <- Schema.element [s]
    case e of
      Elem _ _ [] -> return $ T.pack ""
      _ -> commit $ interior e $ parseSimpleType
  schemaTypeToXML s text = toXMLElement s [] [toXMLText (simpleTypeText text)]

instance SimpleType T.Text where
  acceptingParser = fmap T.pack (Parse.many Parse.next)
  simpleTypeText text = T.unpack text

instance SchemaType a => SchemaType (ArrayOf a) where
  parseSchemaType s = return ArrayOf `apply` many (parseSchemaType s)
  schemaTypeToXML s (ArrayOf xs) = concatMap (schemaTypeToXML s) xs 

instance IsList (ArrayOf a) where
  type Item (ArrayOf a) = a
  fromList xs = ArrayOf xs
  toList (ArrayOf xs) = xs

-- XML instances for general types
$(deriveXml ''SubscriptionIdList)
$(deriveXml ''ArrayOfString)
$(deriveXml ''ArrayOfNumericString)
$(deriveXml ''NumericString)
$(deriveXml ''MerchantAuthentication)
$(deriveXml ''CustomerType)
$(deriveXml ''NameAndAddress)
$(deriveXml ''CardArt)
$(deriveXml ''CreditCard)
$(deriveXml ''CreditCardMasked)
$(deriveXml ''Decimal)
$(deriveXml ''CustomerAddress)
$(deriveXml ''DriversLicense)
$(deriveXml ''BankAccountType)
$(deriveXml ''EcheckType)
$(deriveXml ''TransactionType)
$(deriveXml ''BankAccount)
$(deriveXml ''BankAccountMasked)
$(deriveXml ''CreditCardTrack)
$(deriveXml ''EncryptedTrackData)
$(deriveXml ''PayPal)
$(deriveXml ''OpaqueData)
$(deriveXml ''PaymentEmv)
$(deriveXml ''Payment)
$(deriveXml ''TokenMasked)
$(deriveXml ''PaymentMasked)
$(deriveXml ''PaymentProfile)
$(deriveXml ''CustomerPaymentProfile)
$(deriveXml ''CustomerPaymentProfileEx)
$(deriveXml ''CustomerPaymentProfileMasked)
$(deriveXml ''CustomerPaymentProfileSearchType)
$(deriveXml ''CustomerPaymentProfileOrderFieldEnum)
$(deriveXml ''CustomerPaymentProfileSorting)
$(deriveXml ''Paging)
$(deriveXml ''CustomerPaymentProfileListItem)
$(deriveXml ''ArrayOfCustomerPaymentProfileListItem)
$(deriveXml ''CustomerProfileBase)
$(deriveXml ''CustomerProfile)
$(deriveXml ''CustomerProfileEx)
$(deriveXml ''CustomerAddressEx)
$(deriveXml ''CustomerProfileMasked)
$(deriveXml ''ValidationMode)
$(deriveXml ''CustomerProfilePayment)
$(deriveXml ''Solution)
$(deriveXml ''Order)
$(deriveXml ''LineItem)
$(deriveXml ''LineItems)
$(deriveXml ''ExtendedAmount)
$(deriveXml ''CustomerData)
$(deriveXml ''CcAuthentication)
$(deriveXml ''TransRetailInfo)
$(deriveXml ''SettingName)
$(deriveXml ''Setting)
$(deriveXml ''ArrayOfSetting)
$(deriveXml ''UserField)
$(deriveXml ''ArrayOfUserField)
$(deriveXml ''SecureAcceptance)
$(deriveXml ''EmvResponse)
$(deriveXml ''TransactionRequest)
$(deriveXml ''MessageType)
$(deriveXml ''Message)
$(deriveXml ''Messages)
$(deriveXml ''PrePaidCard)
$(deriveXml ''TransactionResponse_message)
$(deriveXml ''ArrayOfTransactionResponseMessage)
$(deriveXml ''TransactionResponse_error)
$(deriveXml ''ArrayOfTransactionResponseError)
$(deriveXml ''TransactionResponse_splitTenderPayment)
$(deriveXml ''ArrayOfTransactionResponseSplitTenderPayment)
$(deriveXmlFull ''ANetApiResponse)

-- Instances for requests

$(deriveXml ''AuthenticateTestRequest)
$(deriveXml ''CreateCustomerProfileRequest)
$(deriveXml ''GetCustomerProfileRequest)
$(deriveXml ''GetCustomerProfileIdsRequest)
$(deriveXml ''UpdateCustomerProfileRequest)
$(deriveXml ''DeleteCustomerProfileRequest)
$(deriveXml ''CreateCustomerPaymentProfileRequest)
$(deriveXml ''GetCustomerPaymentProfileRequest)
$(deriveXml ''GetCustomerPaymentProfileListRequest)
$(deriveXml ''ValidateCustomerPaymentProfileRequest)
$(deriveXml ''UpdateCustomerPaymentProfileRequest)
$(deriveXml ''DeleteCustomerPaymentProfileRequest)
$(deriveXml ''CreateCustomerProfileFromTransactionRequest)
$(deriveXml ''GetHostedProfilePageRequest)
$(deriveXml ''CreateTransactionRequest)


instance ApiRequest AuthenticateTestRequest where
  type ResponseType AuthenticateTestRequest = AuthenticateTestResponse

instance ApiRequest CreateCustomerProfileRequest where
  type ResponseType CreateCustomerProfileRequest = CreateCustomerProfileResponse
  
instance ApiRequest GetCustomerProfileRequest where
  type ResponseType GetCustomerProfileRequest = GetCustomerProfileResponse
  
instance ApiRequest GetCustomerProfileIdsRequest where
  type ResponseType GetCustomerProfileIdsRequest = GetCustomerProfileIdsResponse
  
instance ApiRequest UpdateCustomerProfileRequest where
  type ResponseType UpdateCustomerProfileRequest = UpdateCustomerProfileResponse
  
instance ApiRequest DeleteCustomerProfileRequest where
  type ResponseType DeleteCustomerProfileRequest = DeleteCustomerProfileResponse
  
instance ApiRequest CreateCustomerPaymentProfileRequest where
  type ResponseType CreateCustomerPaymentProfileRequest = CreateCustomerPaymentProfileResponse
  
instance ApiRequest GetCustomerPaymentProfileRequest where
  type ResponseType GetCustomerPaymentProfileRequest = GetCustomerPaymentProfileResponse
  
instance ApiRequest GetCustomerPaymentProfileListRequest where
  type ResponseType GetCustomerPaymentProfileListRequest = GetCustomerPaymentProfileListResponse
  
instance ApiRequest ValidateCustomerPaymentProfileRequest where
  type ResponseType ValidateCustomerPaymentProfileRequest = ValidateCustomerPaymentProfileResponse
  
instance ApiRequest UpdateCustomerPaymentProfileRequest where
  type ResponseType UpdateCustomerPaymentProfileRequest = UpdateCustomerPaymentProfileResponse
  
instance ApiRequest DeleteCustomerPaymentProfileRequest where
  type ResponseType DeleteCustomerPaymentProfileRequest = DeleteCustomerPaymentProfileResponse
  
instance ApiRequest CreateCustomerProfileFromTransactionRequest where
  type ResponseType CreateCustomerProfileFromTransactionRequest = CreateCustomerProfileResponse

instance ApiRequest GetHostedProfilePageRequest where
  type ResponseType GetHostedProfilePageRequest = GetHostedProfilePageResponse

instance ApiRequest CreateTransactionRequest where
  type ResponseType CreateTransactionRequest = CreateTransactionResponse

-- Instances for response types


$(deriveXmlFull ''AuthenticateTestResponse)
$(deriveXml ''CreateCustomerProfileResponse)
$(deriveXmlFull ''GetCustomerProfileResponse)
$(deriveXml ''GetCustomerProfileIdsResponse)
$(deriveXml ''UpdateCustomerProfileResponse)
$(deriveXml ''DeleteCustomerProfileResponse)
$(deriveXml ''CreateCustomerPaymentProfileResponse)
$(deriveXmlFull ''GetCustomerPaymentProfileResponse)
$(deriveXmlFull ''GetCustomerPaymentProfileListResponse)
$(deriveXml ''ValidateCustomerPaymentProfileResponse)
$(deriveXml ''UpdateCustomerPaymentProfileResponse)
$(deriveXml ''DeleteCustomerPaymentProfileResponse)
$(deriveXml ''GetHostedProfilePageResponse)
$(deriveXml ''CreateProfileResponse)
$(deriveXml ''TransactionResponse)
$(deriveXml ''CreateTransactionResponse)


instance ApiResponse AuthenticateTestResponse where
  aNetApiResponse (AuthenticateTestResponse{..}) = ANetApiResponse authenticateTestResponse_refId authenticateTestResponse_messages authenticateTestResponse_sessionToken
  
instance ApiResponse CreateCustomerProfileResponse where
  aNetApiResponse (CreateCustomerProfileResponse{..}) = ANetApiResponse createCustomerProfileResponse_refId createCustomerProfileResponse_messages createCustomerProfileResponse_sessionToken
  
instance ApiResponse GetCustomerProfileResponse where
  aNetApiResponse (GetCustomerProfileResponse{..}) = ANetApiResponse getCustomerProfileResponse_refId getCustomerProfileResponse_messages getCustomerProfileResponse_sessionToken
  
instance ApiResponse GetCustomerProfileIdsResponse where
  aNetApiResponse (GetCustomerProfileIdsResponse{..}) = ANetApiResponse getCustomerProfileIdsResponse_refId getCustomerProfileIdsResponse_messages getCustomerProfileIdsResponse_sessionToken
  
instance ApiResponse UpdateCustomerProfileResponse where
  aNetApiResponse (UpdateCustomerProfileResponse{..}) = ANetApiResponse updateCustomerProfileResponse_refId updateCustomerProfileResponse_messages updateCustomerProfileResponse_sessionToken
  
instance ApiResponse DeleteCustomerProfileResponse where
  aNetApiResponse (DeleteCustomerProfileResponse{..}) = ANetApiResponse deleteCustomerProfileResponse_refId deleteCustomerProfileResponse_messages deleteCustomerProfileResponse_sessionToken
  
instance ApiResponse CreateCustomerPaymentProfileResponse where
  aNetApiResponse (CreateCustomerPaymentProfileResponse{..}) = ANetApiResponse createCustomerPaymentProfileResponse_refId createCustomerPaymentProfileResponse_messages createCustomerPaymentProfileResponse_sessionToken
  
instance ApiResponse GetCustomerPaymentProfileResponse where
  aNetApiResponse (GetCustomerPaymentProfileResponse{..}) = ANetApiResponse getCustomerPaymentProfileResponse_refId getCustomerPaymentProfileResponse_messages getCustomerPaymentProfileResponse_sessionToken
  
instance ApiResponse GetCustomerPaymentProfileListResponse where
  aNetApiResponse (GetCustomerPaymentProfileListResponse{..}) = ANetApiResponse getCustomerPaymentProfileListResponse_refId getCustomerPaymentProfileListResponse_messages getCustomerPaymentProfileListResponse_sessionToken
  
instance ApiResponse ValidateCustomerPaymentProfileResponse where
  aNetApiResponse (ValidateCustomerPaymentProfileResponse{..}) = ANetApiResponse validateCustomerPaymentProfileResponse_refId validateCustomerPaymentProfileResponse_messages validateCustomerPaymentProfileResponse_sessionToken
  
instance ApiResponse UpdateCustomerPaymentProfileResponse where
  aNetApiResponse (UpdateCustomerPaymentProfileResponse{..}) = ANetApiResponse updateCustomerPaymentProfileResponse_refId updateCustomerPaymentProfileResponse_messages updateCustomerPaymentProfileResponse_sessionToken

instance ApiResponse DeleteCustomerPaymentProfileResponse where
  aNetApiResponse (DeleteCustomerPaymentProfileResponse{..}) = ANetApiResponse deleteCustomerPaymentProfileResponse_refId deleteCustomerPaymentProfileResponse_messages deleteCustomerPaymentProfileResponse_sessionToken

instance ApiResponse GetHostedProfilePageResponse where
  aNetApiResponse (GetHostedProfilePageResponse{..}) = ANetApiResponse getHostedProfilePageResponse_refId getHostedProfilePageResponse_messages getHostedProfilePageResponse_sessionToken

instance ApiResponse CreateTransactionResponse where
  aNetApiResponse (CreateTransactionResponse{..}) = ANetApiResponse createTransactionResponse_refId createTransactionResponse_messages createTransactionResponse_sessionToken
