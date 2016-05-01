{-# LANGUAGE TemplateHaskell, RecordWildCards,ScopedTypeVariables,FlexibleContexts #-}

module Network.AuthorizeNet.Response where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import Network.AuthorizeNet.Request
import Network.AuthorizeNet.TH
import Network.AuthorizeNet.Types
import Network.AuthorizeNet.Util

-- | The API responses are documented at http://developer.authorize.net/api/reference/index.html
data AuthenticateTestResponse = AuthenticateTestResponse {
  authenticateTestResponse_refId        :: Maybe T.Text,
  authenticateTestResponse_messages     :: Messages,
  authenticateTestResponse_sessionToken :: Maybe T.Text
  } deriving (Eq, Show)

data CreateCustomerProfileResponse = CreateCustomerProfileResponse {
  createCustomerProfileResponse_refId                         :: Maybe T.Text,
  createCustomerProfileResponse_messages                      :: Messages,
  createCustomerProfileResponse_sessionToken                  :: Maybe T.Text,
  -- | The CustomerProfileId should be present on success. Save this for later.
  createCustomerProfileResponse_customerProfileId             :: Maybe CustomerProfileId,
  createCustomerProfileResponse_customerPaymentProfileIdList  :: ArrayOfNumericString,
  createCustomerProfileResponse_customerShippingAddressIdList :: ArrayOfNumericString,
  -- | I believe these are returned by the bank when Authorize.NET attempts to validate the information
  createCustomerProfileResponse_validationDirectResponseList  :: ArrayOfString
  } deriving (Eq, Show)

mkAuthenticateTestResponse :: Messages -> AuthenticateTestResponse
mkAuthenticateTestResponse messages = AuthenticateTestResponse Nothing messages Nothing

data GetCustomerProfileResponse = GetCustomerProfileResponse {
  getCustomerProfileResponse_refId           :: Maybe T.Text,
  getCustomerProfileResponse_messages        :: Messages,
  getCustomerProfileResponse_sessionToken    :: Maybe T.Text,
  
  getCustomerProfileResponse_profile         :: CustomerProfileMasked,
  getCustomerProfileResponse_subscriptionIds :: Maybe SubscriptionIdList
  } deriving (Eq, Show)

data GetCustomerProfileIdsResponse = GetCustomerProfileIdsResponse {
  getCustomerProfileIdsResponse_refId        :: Maybe T.Text,
  getCustomerProfileIdsResponse_messages     :: Messages,
  getCustomerProfileIdsResponse_sessionToken :: Maybe T.Text,

  getCustomerProfileIdsResponse_ids          :: ArrayOfNumericString
  } deriving (Eq, Show)

data UpdateCustomerProfileResponse = UpdateCustomerProfileResponse {
  updateCustomerProfileResponse_refId        :: Maybe T.Text,
  updateCustomerProfileResponse_messages     :: Messages,
  updateCustomerProfileResponse_sessionToken :: Maybe T.Text
  } deriving (Eq, Show)

data DeleteCustomerProfileResponse = DeleteCustomerProfileResponse {
  deleteCustomerProfileResponse_refId        :: Maybe T.Text,
  deleteCustomerProfileResponse_messages     :: Messages,
  deleteCustomerProfileResponse_sessionToken :: Maybe T.Text
  } deriving (Eq, Show)

data CreateCustomerPaymentProfileResponse = CreateCustomerPaymentProfileResponse {
  createCustomerPaymentProfileResponse_refId        :: Maybe T.Text,
  createCustomerPaymentProfileResponse_messages     :: Messages,
  createCustomerPaymentProfileResponse_sessionToken :: Maybe T.Text,

  createCustomerPaymentProfileResponse_customerPaymentProfileId :: Maybe CustomerPaymentProfileId,
  createCustomerPaymentProfileResponse_validationDirectResponse :: Maybe T.Text
  } deriving (Eq, Show)

data GetCustomerPaymentProfileResponse = GetCustomerPaymentProfileResponse {
  getCustomerPaymentProfileResponse_refId        :: Maybe T.Text,
  getCustomerPaymentProfileResponse_messages     :: Messages,
  getCustomerPaymentProfileResponse_sessionToken :: Maybe T.Text,

  getCustomerPaymentProfileResponse_paymentProfile :: Maybe CustomerPaymentProfileMasked
  } deriving (Eq, Show)

data GetCustomerPaymentProfileListResponse = GetCustomerPaymentProfileListResponse {
  getCustomerPaymentProfileListResponse_refId        :: Maybe T.Text,
  getCustomerPaymentProfileListResponse_messages     :: Messages,
  getCustomerPaymentProfileListResponse_sessionToken :: Maybe T.Text,

  getCustomerPaymentProfileListResponse_totalNumInResultSet :: NumericString,
  getCustomerPaymentProfileListResponse_paymentProfiles     :: Maybe ArrayOfCustomerPaymentProfileListItem
  } deriving (Eq, Show)

data ValidateCustomerPaymentProfileResponse = ValidateCustomerPaymentProfileResponse {
  validateCustomerPaymentProfileResponse_refId        :: Maybe T.Text,
  validateCustomerPaymentProfileResponse_messages     :: Messages,
  validateCustomerPaymentProfileResponse_sessionToken :: Maybe T.Text,

  validateCustomerPaymentProfileResponse_directResponse :: Maybe T.Text
  } deriving (Eq, Show)

data UpdateCustomerPaymentProfileResponse = UpdateCustomerPaymentProfileResponse {
  updateCustomerPaymentProfileResponse_refId        :: Maybe T.Text,
  updateCustomerPaymentProfileResponse_messages     :: Messages,
  updateCustomerPaymentProfileResponse_sessionToken :: Maybe T.Text,

  updateCustomerPaymentProfileResponse_validationDirectResponse :: Maybe T.Text
  } deriving (Eq, Show)

data DeleteCustomerPaymentProfileResponse = DeleteCustomerPaymentProfileResponse {
  deleteCustomerPaymentProfileResponse_refId        :: Maybe T.Text,
  deleteCustomerPaymentProfileResponse_messages     :: Messages,
  deleteCustomerPaymentProfileResponse_sessionToken :: Maybe T.Text
  } deriving (Eq, Show)

data GetHostedProfilePageResponse = GetHostedProfilePageResponse {
  getHostedProfilePageResponse_refId        :: Maybe T.Text,
  getHostedProfilePageResponse_messages     :: Messages,
  getHostedProfilePageResponse_sessionToken :: Maybe T.Text,

  getHostedProfilePageResponse_token        :: Maybe T.Text
  } deriving (Eq, Show)

data CreateProfileResponse = CreateProfileResponse {
  createProfileResponse_refId                         :: Maybe T.Text,
  createProfileResponse_messages                      :: Messages,
  createProfileResponse_sessionToken                  :: Maybe T.Text,
  createProfileResponse_customerProfileId             :: Maybe CustomerProfileId,
  createProfileResponse_customerPaymentProfileIdList  :: Maybe (ArrayOfNumericString),
  createProfileResponse_customerShippingAddressIdList :: Maybe (ArrayOfNumericString)
  } deriving (Eq, Show)

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
  -- | TODO: The Authorize.NET XSD suggests there should be a 'messages' field that holds an array of 'message' objects, but I didn't observe that in the output.
  transactionResponse_message             :: Maybe TransactionResponse_message,
  transactionResponse_errors              :: Maybe ArrayOfTransactionResponseError,
  transactionResponse_splitTenderPayments :: Maybe ArrayOfTransactionResponseSplitTenderPayment,
  transactionResponse_userFields          :: Maybe ArrayOfUserField,
  transactionResponse_shipTo              :: Maybe NameAndAddress,
  transactionResponse_secureAcceptance    :: Maybe SecureAcceptance,
  transactionResponse_emvResponse         :: Maybe EmvResponse
  } deriving (Eq, Show)

mkTransactionResponse :: TransactionResponse
mkTransactionResponse = TransactionResponse Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing  Nothing Nothing  Nothing Nothing  Nothing Nothing  Nothing Nothing  Nothing Nothing  Nothing Nothing Nothing Nothing 

data CreateTransactionResponse = CreateTransactionResponse {
  createTransactionResponse_refId        :: Maybe T.Text,
  createTransactionResponse_messages     :: Messages,
  createTransactionResponse_sessionToken :: Maybe T.Text,

  createTransactionResponse_transactionResponse :: TransactionResponse,
  createTransactionResponse_profileResponse     :: Maybe CreateProfileResponse
  } deriving (Eq, Show)
