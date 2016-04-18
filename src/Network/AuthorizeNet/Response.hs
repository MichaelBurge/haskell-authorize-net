{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

module Network.AuthorizeNet.Response where

import Data.Aeson
import Data.Aeson.TH

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import Network.AuthorizeNet.Request
import Network.AuthorizeNet.TH
import Network.AuthorizeNet.Types

-- | The API responses are documented at http://developer.authorize.net/api/reference/index.html
data AuthenticateTestResponse = AuthenticateTestResponse {
  authenticateTestResponse_refId        :: Maybe T.Text,
  authenticateTestResponse_messages     :: Messages,
  authenticateTestResponse_sessionToken :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''AuthenticateTestResponse)

data CreateCustomerProfileResponse = CreateCustomerProfileResponse {
  createCustomerProfileResponse_refId                         :: Maybe T.Text,
  createCustomerProfileResponse_messages                      :: Messages,
  createCustomerProfileResponse_sessionToken                  :: Maybe T.Text,
  -- | The CustomerProfileId should be present on success. Save this for later.
  createCustomerProfileResponse_customerProfileId             :: Maybe CustomerProfileId,
  createCustomerProfileResponse_customerPaymentProfileIdList  :: [CustomerPaymentProfileId],
  createCustomerProfileResponse_customerShippingAddressIdList :: [CustomerShippingAddressId],
  -- | I believe these are returned by the bank when Authorize.NET attempts to validate the information
  createCustomerProfileResponse_validationDirectResponseList  :: [T.Text]
  } deriving (Eq, Show)
                   
$(deriveJSON dropRecordName ''CreateCustomerProfileResponse)

mkAuthenticateTestResponse :: Messages -> AuthenticateTestResponse
mkAuthenticateTestResponse messages = AuthenticateTestResponse Nothing messages Nothing

data GetCustomerProfileResponse = GetCustomerProfileResponse {
  getCustomerProfileResponse_refId           :: Maybe T.Text,
  getCustomerProfileResponse_messages        :: Messages,
  getCustomerProfileResponse_sessionToken    :: Maybe T.Text,
  
  getCustomerProfileResponse_profile         :: CustomerProfileMasked,
  getCustomerProfileResponse_subscriptionIds :: ArrayOf SubscriptionId
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''GetCustomerProfileResponse)

data GetCustomerProfileIdsResponse = GetCustomerProfileIdsResponse {
  getCustomerProfileIdsResponse_refId        :: Maybe T.Text,
  getCustomerProfileIdsResponse_messages     :: Messages,
  getCustomerProfileIdsResponse_sessionToken :: Maybe T.Text,

  getCustomerProfileIdsResponse_ids          :: ArrayOf CustomerProfileId
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''GetCustomerProfileIdsResponse)

data UpdateCustomerProfileResponse = UpdateCustomerProfileResponse {
  updateCustomerProfileResponse_refId        :: Maybe T.Text,
  updateCustomerProfileResponse_messages     :: Messages,
  updateCustomerProfileResponse_sessionToken :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''UpdateCustomerProfileResponse)

data DeleteCustomerProfileResponse = DeleteCustomerProfileResponse {
  deleteCustomerProfileResponse_refId        :: Maybe T.Text,
  deleteCustomerProfileResponse_messages     :: Messages,
  deleteCustomerProfileResponse_sessionToken :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''DeleteCustomerProfileResponse)

data CreateCustomerPaymentProfileResponse = CreateCustomerPaymentProfileResponse {
  createCustomerPaymentProfileResponse_refId        :: Maybe T.Text,
  createCustomerPaymentProfileResponse_messages     :: Messages,
  createCustomerPaymentProfileResponse_sessionToken :: Maybe T.Text,

  createCustomerPaymentProfileResponse_customerPaymentProfileId :: Maybe CustomerPaymentProfileId,
  createCustomerPaymentProfileResponse_validationDirectResponse :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''CreateCustomerPaymentProfileResponse)

data GetCustomerPaymentProfileResponse = GetCustomerPaymentProfileResponse {
  getCustomerPaymentProfileResponse_refId        :: Maybe T.Text,
  getCustomerPaymentProfileResponse_messages     :: Messages,
  getCustomerPaymentProfileResponse_sessionToken :: Maybe T.Text,

  getCustomerPaymentProfileResponse_paymentProfile :: Maybe CustomerPaymentProfileMasked
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''GetCustomerPaymentProfileResponse)

data GetCustomerPaymentProfileListResponse = R_getCustomerPaymentProfileListResponse {
  getCustomerPaymentProfileListResponse_refId        :: Maybe T.Text,
  getCustomerPaymentProfileListResponse_messages     :: Messages,
  getCustomerPaymentProfileListResponse_sessionToken :: Maybe T.Text,

  getCustomerPaymentProfileListResponse_totalNumInResultSet :: NumericString,
  getCustomerPaymentProfileListResponse_paymentProfiles     :: Maybe ArrayOfCustomerPaymentProfileListItem
  } | R_DummyConstructorForAeson deriving (Eq, Show)

$(deriveJSON choiceType ''GetCustomerPaymentProfileListResponse)

data ValidateCustomerPaymentProfileResponse = ValidateCustomerPaymentProfileResponse {
  validateCustomerPaymentProfileResponse_refId        :: Maybe T.Text,
  validateCustomerPaymentProfileResponse_messages     :: Messages,
  validateCustomerPaymentProfileResponse_sessionToken :: Maybe T.Text,

  validateCustomerPaymentProfileResponse_directResponse :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''ValidateCustomerPaymentProfileResponse)

data UpdateCustomerPaymentProfileResponse = UpdateCustomerPaymentProfileResponse {
  updateCustomerPaymentProfileResponse_refId        :: Maybe T.Text,
  updateCustomerPaymentProfileResponse_messages     :: Messages,
  updateCustomerPaymentProfileResponse_sessionToken :: Maybe T.Text,

  updateCustomerPaymentProfileResponse_validationDirectResponse :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''UpdateCustomerPaymentProfileResponse)

data DeleteCustomerPaymentProfileResponse = DeleteCustomerPaymentProfileResponse {
  deleteCustomerPaymentProfileResponse_refId        :: Maybe T.Text,
  deleteCustomerPaymentProfileResponse_messages     :: Messages,
  deleteCustomerPaymentProfileResponse_sessionToken :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''DeleteCustomerPaymentProfileResponse)

data GetHostedProfilePageResponse = GetHostedProfilePageResponse {
  getHostedProfilePageResponse_refId        :: Maybe T.Text,
  getHostedProfilePageResponse_messages     :: Messages,
  getHostedProfilePageResponse_sessionToken :: Maybe T.Text,

  getHostedProfilePageResponse_token        :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''GetHostedProfilePageResponse)

data CreateProfileResponse = CreateProfileResponse {
  createProfileResponse_refId                         :: Maybe T.Text,
  createProfileResponse_messages                      :: Messages,
  createProfileResponse_sessionToken                  :: Maybe T.Text,
  createProfileResponse_customerProfileId             :: Maybe CustomerProfileId,
  createProfileResponse_customerPaymentProfileIdList  :: Maybe (ArrayOf CustomerPaymentProfileId),
  createProfileResponse_customerShippingAddressIdList :: Maybe (ArrayOf CustomerShippingAddressId)
  } deriving (Eq, Show)
                             
$(deriveJSON dropRecordName ''CreateProfileResponse)

data TransactionResponse = TransactionResponse {
  transactionResponse_responseCode        :: Maybe T.Text,
  transactionResponse_rawResponseCode     :: Maybe T.Text,
  transactionResponse_authCode            :: Maybe T.Text,
  transactionResponse_avsResultCode       :: Maybe T.Text,
  transactionResponse_cvvResultCode       :: Maybe T.Text,
  transactionResponse_cavvResultCode      :: Maybe T.Text,
  transactionResponse_transId             :: Maybe T.Text,
  transactionResponse_refTransID          :: Maybe T.Text,
  transactionResponse_transHash           :: Maybe T.Text,
  transactionResponse_testRequest         :: Maybe T.Text,
  transactionResponse_accountNumber       :: Maybe T.Text,
  transactionResponse_accountType         :: Maybe T.Text,
  transactionResponse_entryMode           :: Maybe T.Text,
  transactionResponse_splitTenderId       :: Maybe T.Text,
  transactionResponse_prePaidCard         :: Maybe PrePaidCard,
  transactionResponse_messages            :: Maybe (ArrayOf TransactionResponse_message),
  transactionResponse_errors              :: Maybe (ArrayOf TransactionResponse_error),
  transactionResponse_splitTenderPayments :: Maybe (ArrayOf TransactionResponse_splitTenderPayment),
  transactionResponse_userFields          :: Maybe (ArrayOf UserField),
  transactionResponse_shipTo              :: Maybe NameAndAddress,
  transactionResponse_secureAcceptance    :: Maybe SecureAcceptance,
  transactionResponse_emvResponse         :: Maybe EmvResponse
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''TransactionResponse)

mkTransactionResponse :: TransactionResponse
mkTransactionResponse = TransactionResponse Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing  Nothing Nothing  Nothing Nothing  Nothing Nothing  Nothing Nothing  Nothing Nothing  Nothing Nothing Nothing Nothing 

data CreateTransactionResponse = CreateTransactionResponse {
  createTransactionResponse_refId        :: Maybe T.Text,
  createTransactionResponse_messages     :: Messages,
  createTransactionResponse_sessionToken :: Maybe T.Text,

  createTransactionResponse_transactionResponse :: TransactionResponse,
  createTransactionResponse_profileResponse     :: Maybe CreateProfileResponse
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''CreateTransactionResponse)

data ApiResponse = R_AuthenticateTest AuthenticateTestResponse
                 | R_CreateCustomerProfile CreateCustomerProfileResponse
                 | R_GetCustomerProfile GetCustomerProfileResponse
                 | R_GetCustomerProfileIds GetCustomerProfileIdsResponse
                 | R_UpdateCustomerProfile UpdateCustomerProfileResponse
                 | R_DeleteCustomerProfile DeleteCustomerProfileResponse
                 | R_CreateCustomerPaymentProfile CreateCustomerPaymentProfileResponse
                 | R_GetCustomerPaymentProfile GetCustomerPaymentProfileResponse
                 | R_GetCustomerPaymentProfileList GetCustomerPaymentProfileListResponse
                 | R_ValidateCustomerPaymentProfile ValidateCustomerPaymentProfileResponse
                 | R_UpdateCustomerPaymentProfile UpdateCustomerPaymentProfileResponse
                 | R_DeleteCustomerPaymentProfile DeleteCustomerPaymentProfileResponse
                 | R_CreateProfile CreateProfileResponse
                 | R_GetHostedProfilePage GetHostedProfilePageResponse
                 | R_CreateTransaction CreateTransactionResponse
                 deriving (Eq, Show)

-- | Decodes a bytestring into the appropriate response given a request
decodeRequestResponse :: ApiRequest -> BSL.ByteString -> Either String ApiResponse
decodeRequestResponse apiRequest bsl = case apiRequest of
  AuthenticateTest{}               -> R_AuthenticateTest               <$> eitherDecode' bsl
  CreateCustomerProfile{}          -> R_CreateCustomerProfile          <$> eitherDecode' bsl
  GetCustomerProfile{}             -> R_GetCustomerProfile             <$> eitherDecode' bsl
  GetCustomerProfileIds{}          -> R_GetCustomerProfileIds          <$> eitherDecode' bsl
  UpdateCustomerProfile{}          -> R_UpdateCustomerProfile          <$> eitherDecode' bsl
  DeleteCustomerProfile{}          -> R_DeleteCustomerProfile          <$> eitherDecode' bsl
  CreateCustomerPaymentProfile{}   -> R_CreateCustomerPaymentProfile   <$> eitherDecode' bsl
  GetCustomerPaymentProfile{}      -> R_GetCustomerPaymentProfile      <$> eitherDecode' bsl
  GetCustomerPaymentProfileList{}  -> R_GetCustomerPaymentProfileList  <$> eitherDecode' bsl
  ValidateCustomerPaymentProfile{} -> R_ValidateCustomerPaymentProfile <$> eitherDecode' bsl
  UpdateCustomerPaymentProfile{}   -> R_UpdateCustomerPaymentProfile   <$> eitherDecode' bsl
  DeleteCustomerPaymentProfile{}   -> R_DeleteCustomerPaymentProfile   <$> eitherDecode' bsl
  CreateCustomerProfile{}          -> R_CreateProfile                  <$> eitherDecode' bsl
  GetHostedProfilePage{}           -> R_GetHostedProfilePage           <$> eitherDecode' bsl
  CreateTransaction{}              -> R_CreateTransaction              <$> eitherDecode' bsl

-- | anet:ANetApiResponse
data ANetApiResponse = ANetApiResponse {
  aNetApiResponse_refId        :: Maybe T.Text,
  aNetApiResponse_messages     :: Messages,
  aNetApiResponse_sessionToken :: Maybe T.Text
  } deriving (Eq, Show)

-- | All Response records should be 'extensions' of the ANetApiResponse type, but Haskell's type system doesn't support that so we duplicate the fields on each type. This maps the duplicated fields back onto an instance of the extended type for easier use.
response_aNetApiResponse :: ApiResponse -> ANetApiResponse
response_aNetApiResponse (R_AuthenticateTest AuthenticateTestResponse{..}) = ANetApiResponse authenticateTestResponse_refId authenticateTestResponse_messages authenticateTestResponse_sessionToken
response_aNetApiResponse (R_CreateCustomerProfile CreateCustomerProfileResponse{..}) = ANetApiResponse createCustomerProfileResponse_refId createCustomerProfileResponse_messages createCustomerProfileResponse_sessionToken
response_aNetApiResponse (R_GetCustomerProfile GetCustomerProfileResponse{..}) = ANetApiResponse getCustomerProfileResponse_refId getCustomerProfileResponse_messages getCustomerProfileResponse_sessionToken
response_aNetApiResponse (R_GetCustomerProfileIds GetCustomerProfileIdsResponse{..}) = ANetApiResponse getCustomerProfileIdsResponse_refId getCustomerProfileIdsResponse_messages getCustomerProfileIdsResponse_sessionToken
response_aNetApiResponse (R_UpdateCustomerProfile UpdateCustomerProfileResponse{..}) = ANetApiResponse updateCustomerProfileResponse_refId updateCustomerProfileResponse_messages updateCustomerProfileResponse_sessionToken
response_aNetApiResponse (R_DeleteCustomerProfile DeleteCustomerProfileResponse{..}) = ANetApiResponse deleteCustomerProfileResponse_refId deleteCustomerProfileResponse_messages deleteCustomerProfileResponse_sessionToken
response_aNetApiResponse (R_CreateCustomerPaymentProfile CreateCustomerPaymentProfileResponse{..}) = ANetApiResponse createCustomerPaymentProfileResponse_refId createCustomerPaymentProfileResponse_messages createCustomerPaymentProfileResponse_sessionToken
response_aNetApiResponse (R_GetCustomerPaymentProfile GetCustomerPaymentProfileResponse{..}) = ANetApiResponse getCustomerPaymentProfileResponse_refId getCustomerPaymentProfileResponse_messages getCustomerPaymentProfileResponse_sessionToken
response_aNetApiResponse (R_GetCustomerPaymentProfileList R_getCustomerPaymentProfileListResponse{..}) = ANetApiResponse getCustomerPaymentProfileListResponse_refId getCustomerPaymentProfileListResponse_messages getCustomerPaymentProfileListResponse_sessionToken
response_aNetApiResponse (R_ValidateCustomerPaymentProfile ValidateCustomerPaymentProfileResponse{..}) = ANetApiResponse validateCustomerPaymentProfileResponse_refId validateCustomerPaymentProfileResponse_messages validateCustomerPaymentProfileResponse_sessionToken
response_aNetApiResponse (R_UpdateCustomerPaymentProfile UpdateCustomerPaymentProfileResponse{..}) = ANetApiResponse updateCustomerPaymentProfileResponse_refId updateCustomerPaymentProfileResponse_messages updateCustomerPaymentProfileResponse_sessionToken
response_aNetApiResponse (R_DeleteCustomerPaymentProfile DeleteCustomerPaymentProfileResponse{..}) = ANetApiResponse deleteCustomerPaymentProfileResponse_refId deleteCustomerPaymentProfileResponse_messages deleteCustomerPaymentProfileResponse_sessionToken
response_aNetApiResponse (R_CreateProfile CreateProfileResponse{..}) = ANetApiResponse Nothing createProfileResponse_messages Nothing
response_aNetApiResponse (R_GetHostedProfilePage GetHostedProfilePageResponse{..}) = ANetApiResponse getHostedProfilePageResponse_refId getHostedProfilePageResponse_messages getHostedProfilePageResponse_sessionToken
response_aNetApiResponse (R_CreateTransaction CreateTransactionResponse{..}) = ANetApiResponse createTransactionResponse_refId createTransactionResponse_messages createTransactionResponse_sessionToken
