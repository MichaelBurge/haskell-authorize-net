{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module Network.AuthorizeNet.Request where

import GHC.Generics

import qualified Data.Text as T

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

  --  | GetCustomerPaymentProfile {
  -- getCustomerPaymentProfile_merchantAuthentication   :: MerchantAuthentication,
  -- getCustomerPaymentProfile_customerProfileId        :: CustomerProfileId,
  -- getCustomerPaymentProfile_customerPaymentProfileId :: CustomerPaymentProfileId
  -- } | GetCustomerPaymentProfileList {
  -- getCustomerPaymentProfileList_merchantAuthentication :: MerchantAuthentication,
  -- getCustomerPaymentProfileList_searchtype             :: CustomerPaymentProfileSearchType,
  -- getCustomerPaymentProfileList_month                  :: T.Text,
  -- getCustomerPaymentProfileList_sorting                :: CustomerPaymentProfileSorting,
  -- getCustomerPaymentProfileList_paging                 :: Paging
  -- } | ValidateCustomerPaymentProfile {
  -- validateCustomerPaymentProfile_merchantAuthentication    :: MerchantAuthentication,
  -- validateCustomerPaymentProfile_customerProfileId         :: CustomerProfileId,
  -- validateCustomerPaymentProfile_customerPaymentProfileId  :: CustomerPaymentProfileId,
  -- validateCustomerPaymentProfile_customerShippingAddressId :: Maybe CustomerShippingAddressId,
  -- validateCustomerPaymentProfile_cardCode                  :: Maybe CardCode,
  -- validateCustomerPaymentProfile_validationMode            :: ValidationMode
  -- } | UpdateCustomerPaymentProfile {
  -- validateCustomerPaymentProfile_merchantAuthentication :: MerchantAuthentication,
  -- validateCustomerPaymentProfile_customerProfileId      :: CustomerProfileId,
  -- validateCustomerPaymentProfile_paymentProfile         :: CustomerPaymentProfileEx,
  -- validateCustomerPaymentProfile_validationMode         :: ValidationMode
  -- } | DeleteCustomerPaymentProfile {
  -- deleteCustomerPaymentProfile_merchantAuthentication   :: MerchantAuthentication,
  -- deleteCustomerPaymentProfile_customerProfileId        :: CustomerProfileId,
  -- deleteCustomerPaymentProfile_customerPaymentProfileId :: CustomerPaymentProfileId
  -- } | CreateCustomerProfileFromTransaction {
  -- createCustomerProfileFromTransaction_merchantAuthentication :: MerchantAuthentication,
  -- createCustomerProfileFromTransaction_transId                :: TransactionId,
  -- createCustomerProfileFromTransaction_customer               :: Maybe CustomerProfileBase,
  -- createCustomerProfileFromTransaction_customerProfileId      :: Maybe CustomerProfileId
  -- } | GetHostedProfilePage {
  -- getHostedProfilePage_merchantAuthentication :: MerchantAuthentication,
  -- getHostedProfilePage_refId                  :: Maybe T.Text,
  -- getHostedProfilePage_customerProfileId      :: CustomerProfileId,
  -- getHostedProfilePage_hostedProfileSettings  :: Maybe ArrayOfSetting
  -- } | CreateTransaction {
  -- createTransaction_merchantAuthentication :: MerchantAuthentication,
  -- createTransaction_refId                  :: Maybe T.Text,
  -- createTransaction_transactionRequest     :: TransactionRequest
  -- }
-- instance ToJSON ApiRequest where
--   toEncoding = genericToEncoding requestOptions

-- instance FromJSON ApiRequest where
--   parseJSON = genericParseJSON requestOptions
