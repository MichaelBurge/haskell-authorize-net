{-# LANGUAGE TemplateHaskell #-}

module Network.AuthorizeNet.Response where

import Data.Aeson.TH

import qualified Data.Text as T

import Network.AuthorizeNet.Api
import Network.AuthorizeNet.TH

-- | The API responses are documented at http://developer.authorize.net/api/reference/index.html
data AuthenticateTestResponse = AuthenticateTestResponse {
  authenticateTest_refId        :: Maybe T.Text,
  authenticateTest_messages     :: Messages,
  authenticateTest_sessionToken :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''AuthenticateTestResponse)

data CreateCustomerProfileResponse = CreateCustomerProfileResponse {
  createCustomerProfile_refId                         :: Maybe T.Text,
  createCustomerProfile_messages                      :: Messages,
  createCustomerProfile_sessionToken                  :: Maybe T.Text,
  -- | The CustomerProfileId should be present on success. Save this for later.
  createCustomerProfile_customerProfileId             :: Maybe CustomerProfileId,
  createCustomerProfile_customerPaymentProfileIdList  :: [CustomerPaymentProfileId],
  createCustomerProfile_customerShippingAddressIdList :: [CustomerShippingAddressId],
  -- | These aren't interpreted by the type system at all. I'm not really sure what they are, honestly.
  createCustomerProfile_validationDirectResponseList  :: [T.Text]
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

data CreateProfileResponse = CreateProfileResponse {
  createProfileResponse_messages                      :: Messages,
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
mkTransactionResponse = TransactionResponse Nothing Nothing Nothing Nothing Nothing Nothing  Nothing Nothing  Nothing Nothing  Nothing Nothing  Nothing Nothing  Nothing Nothing  Nothing Nothing  Nothing Nothing  Nothing Nothing 

data CreateTransactionResponse = CreateTransactionResponse {
  createTransactionResponse_refId        :: Maybe T.Text,
  createTransactionResponse_messages     :: Messages,
  createTransactionResponse_sessionToken :: Maybe T.Text,

  createTransactionResponse_transactionResponse :: TransactionResponse,
  createTransactionResponse_profileResponse     :: Maybe CreateProfileResponse
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''CreateTransactionResponse)

