{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module Network.AuthorizeNet.Request where

import GHC.Generics

import qualified Data.Text as T

import Network.AuthorizeNet.Instances
import Network.AuthorizeNet.TH
import Network.AuthorizeNet.Types  

-- | The API requests are documented at http://developer.authorize.net/api/reference/index.html
data AuthenticateTestRequest = AuthenticateTestRequest {
  authenticateTest_merchantAuthentication :: MerchantAuthentication
  } deriving (Eq, Show)

$(deriveXml ''AuthenticateTestRequest)

data CreateCustomerProfileRequest = CreateCustomerProfileRequest {
  createCustomerProfile_merchantAuthentication :: MerchantAuthentication,
  createCustomerProfile_profile                :: CustomerProfile,
  createCustomerProfile_validationMode         :: ValidationMode
  } deriving (Eq, Show)

$(deriveXml ''CreateCustomerProfileRequest)

data GetCustomerProfileRequest = GetCustomerProfileRequest {
  getCustomerProfile_merchantAuthentication :: MerchantAuthentication,
  getCustomerProfile_customerProfileId      :: CustomerProfileId
  } deriving (Eq, Show)

$(deriveXml ''GetCustomerProfileRequest)

data GetCustomerProfileIdsRequest = GetCustomerProfileIdsRequest {
  getCustomerProfileIds_merchantAuthentication :: MerchantAuthentication
  } deriving (Eq, Show)

$(deriveXml ''GetCustomerProfileIdsRequest)

data UpdateCustomerProfileRequest = UpdateCustomerProfileRequest {
  updateCustomerProfile_merchantAuthentication :: MerchantAuthentication,
  updateCustomerProfile_profile                :: CustomerProfileEx
  } deriving (Eq, Show)

$(deriveXml ''UpdateCustomerProfileRequest)

data DeleteCustomerProfileRequest = DeleteCustomerProfileRequest {
  deleteCustomerProfile_merchantAuthentication :: MerchantAuthentication,
  deleteCustomerProfile_customerProfileId      :: CustomerProfileId
  } deriving (Eq, Show)

$(deriveXml ''DeleteCustomerProfileRequest)

data CreateCustomerPaymentProfileRequest = CreateCustomerPaymentProfileRequest {
  createCustomerPaymentProfile_merchantAuthentication :: MerchantAuthentication,
  createCustomerPaymentProfile_customerProfileId      :: CustomerProfileId,
  createCustomerPaymentProfile_paymentProfile         :: CustomerPaymentProfile,
  createCustomerPaymentProfile_validationMode         :: ValidationMode
  } deriving (Eq, Show)

$(deriveXml ''CreateCustomerPaymentProfileRequest)

data GetCustomerPaymentProfileRequest = GetCustomerPaymentProfileRequest {
  getCustomerPaymentProfileRequest_merchantAuthentication   :: MerchantAuthentication,
  getCustomerPaymentProfileRequest_customerProfileId        :: CustomerProfileId,
  getCustomerPaymentProfileRequest_customerPaymentProfileId :: CustomerPaymentProfileId
  } deriving (Eq, Show)

$(deriveXml ''GetCustomerPaymentProfileRequest)

data GetCustomerPaymentProfileListRequest = GetCustomerPaymentProfileListRequest {
  getCustomerPaymentProfileListRequest_merchantAuthentication :: MerchantAuthentication,
  getCustomerPaymentProfileListRequest_searchtype             :: CustomerPaymentProfileSearchType,
  getCustomerPaymentProfileListRequest_month                  :: T.Text,
  getCustomerPaymentProfileListRequest_sorting                :: CustomerPaymentProfileSorting,
  getCustomerPaymentProfileListRequest_paging                 :: Paging
  } deriving (Eq, Show)

$(deriveXml ''GetCustomerPaymentProfileListRequest)

data ValidateCustomerPaymentProfileRequest = ValidateCustomerPaymentProfileRequest {
  validateCustomerPaymentProfileRequest_merchantAuthentication    :: MerchantAuthentication,
  validateCustomerPaymentProfileRequest_customerProfileId         :: CustomerProfileId,
  validateCustomerPaymentProfileRequest_customerPaymentProfileId  :: CustomerPaymentProfileId,
  validateCustomerPaymentProfileRequest_customerShippingAddressId :: Maybe CustomerShippingAddressId,
  validateCustomerPaymentProfileRequest_cardCode                  :: Maybe CardCode,
  validateCustomerPaymentProfileRequest_validationMode            :: ValidationMode
  } deriving (Eq, Show)

$(deriveXml ''ValidateCustomerPaymentProfileRequest)

data UpdateCustomerPaymentProfileRequest = UpdateCustomerPaymentProfileRequest {
 updateCustomerPaymentProfileRequest_merchantAuthentication :: MerchantAuthentication,
 updateCustomerPaymentProfileRequest_customerProfileId      :: CustomerProfileId,
 updateCustomerPaymentProfileRequest_paymentProfile         :: CustomerPaymentProfileEx,
 updateCustomerPaymentProfileRequest_validationMode         :: ValidationMode
  } deriving (Eq, Show)

$(deriveXml ''UpdateCustomerPaymentProfileRequest)
                                    
data DeleteCustomerPaymentProfileRequest = DeleteCustomerPaymentProfileRequest {
  deleteCustomerPaymentProfileRequest_merchantAuthentication   :: MerchantAuthentication,
  deleteCustomerPaymentProfileRequest_customerProfileId        :: CustomerProfileId,
  deleteCustomerPaymentProfileRequest_customerPaymentProfileId :: CustomerPaymentProfileId
  } deriving (Eq, Show)

$(deriveXml ''DeleteCustomerPaymentProfileRequest)

data CreateCustomerProfileFromTransactionRequest = CreateCustomerProfileFromTransactionRequest {
  createCustomerProfileFromTransactionRequest_merchantAuthentication :: MerchantAuthentication,
  createCustomerProfileFromTransactionRequest_transId                :: TransactionId,
  createCustomerProfileFromTransactionRequest_customer               :: Maybe CustomerProfileBase,
  createCustomerProfileFromTransactionRequest_customerProfileId      :: Maybe CustomerProfileId
  } deriving (Eq, Show)

$(deriveXml ''CreateCustomerProfileFromTransactionRequest)

data GetHostedProfilePageRequest = GetHostedProfilePageRequest {
  getHostedProfilePageRequest_merchantAuthentication :: MerchantAuthentication,
  getHostedProfilePageRequest_refId                  :: Maybe T.Text,
  getHostedProfilePageRequest_customerProfileId      :: CustomerProfileId,
  getHostedProfilePageRequest_hostedProfileSettings  :: Maybe ArrayOfSetting
  } deriving (Eq, Show)

$(deriveXml ''GetHostedProfilePageRequest)

data CreateTransactionRequest = CreateTransactionRequest {
  createTransactionRequest_merchantAuthentication :: MerchantAuthentication,
  createTransactionRequest_refId                  :: Maybe T.Text,
  createTransactionRequest_transactionRequest     :: TransactionRequest
  } deriving (Eq, Show)

$(deriveXml ''CreateTransactionRequest)
