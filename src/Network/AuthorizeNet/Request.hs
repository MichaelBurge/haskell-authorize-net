{-# LANGUAGE TemplateHaskell #-}

module Network.AuthorizeNet.Request where

import Data.Aeson.TH

import qualified Data.Text as T

import Network.AuthorizeNet.TH
import Network.AuthorizeNet.Types

-- | The API requests are documented at http://developer.authorize.net/api/reference/index.html
data ApiRequest = AuthenticateTest {
  authenticateTest_merchantAuthentication :: MerchantAuthentication
  } | CreateCustomerProfile {
  createCustomerProfile_merchantAuthentication :: MerchantAuthentication,
  createCustomerProfile_profile                :: CustomerProfile
  } | GetCustomerProfile {
  getCustomerProfile_merchantAuthentication :: MerchantAuthentication,
  getCustomerProfile_customerProfileId      :: CustomerProfileId
  } | GetCustomerProfileIds {
  getCustomerProfileIds_merchantAuthentication :: MerchantAuthentication
  } | UpdateCustomerProfile {
  updateCustomerProfile_merchantAuthentication :: MerchantAuthentication,
  updateCustomerProfile_profile                :: CustomerProfile
  } | DeleteCustomerProfile {
  deleteCustomerProfile_merchantAuthentication :: MerchantAuthentication,
  deleteCustomerProfile_customerProfileId      :: CustomerProfileId
  } | CreateCustomerPaymentProfile {
  createCustomerPaymentProfile_merchantAuthentication :: MerchantAuthentication,
  createCustomerPaymentProfile_customerProfileId      :: CustomerProfileId,
  createCustomerPaymentProfile_paymentProfile         :: CustomerPaymentProfile
  } | GetCustomerPaymentProfile {
  getCustomerPaymentProfile_merchantAuthentication   :: MerchantAuthentication,
  getCustomerPaymentProfile_customerProfileId        :: CustomerProfileId,
  getCustomerPaymentProfile_customerPaymentProfileId :: CustomerPaymentProfileId
  } | GetCustomerPaymentProfileList {
  getCustomerPaymentProfileList_merchantAuthentication :: MerchantAuthentication,
  getCustomerPaymentProfileList_searchtype             :: CustomerPaymentProfileSearchType,
  getCustomerPaymentProfileList_month                  :: T.Text,
  getCustomerPaymentProfileList_sorting                :: CustomerPaymentProfileSorting,
  getCustomerPaymentProfileList_paging                 :: Paging
  } | ValidateCustomerPaymentProfile {
  validateCustomerPaymentProfile_merchantAuthentication    :: MerchantAuthentication,
  validateCustomerPaymentProfile_customerProfileId         :: CustomerProfileId,
  validateCustomerPaymentProfile_customerPaymentProfileId  :: CustomerPaymentProfileId,
  validateCustomerPaymentProfile_customerShippingAddressId :: Maybe CustomerShippingAddressId,
  validateCustomerPaymentProfile_cardCode                  :: Maybe CardCode,
  validateCustomerPaymentProfile_validationMode            :: ValidationMode
  } | UpdateCustomerPaymentProfile {
  validateCustomerPaymentProfile_merchantAuthentication :: MerchantAuthentication,
  validateCustomerPaymentProfile_customerProfileId      :: CustomerProfileId,
  validateCustomerPaymentProfile_paymentProfile         :: CustomerPaymentProfileEx,
  validateCustomerPaymentProfile_validationMode         :: ValidationMode
  } | DeleteCustomerPaymentProfile {
  deleteCustomerPaymentProfile_merchantAuthentication   :: MerchantAuthentication,
  deleteCustomerPaymentProfile_customerProfileId        :: CustomerProfileId,
  deleteCustomerPaymentProfile_customerPaymentProfileId :: CustomerPaymentProfileId
  } | CreateCustomerProfileFromTransaction {
  createCustomerProfileFromTransaction_merchantAuthentication :: MerchantAuthentication,
  createCustomerProfileFromTransaction_transId                :: TransactionId,
  createCustomerProfileFromTransaction_customer               :: Maybe CustomerProfileBase,
  createCustomerProfileFromTransaction_customerProfileId      :: Maybe CustomerProfileId
  } | CreateTransaction {
  createTransaction_merchantAuthentication :: MerchantAuthentication,
  createTransaction_refId                  :: Maybe T.Text,
  createTransaction_transactionRequest     :: TransactionRequest
  }
  deriving (Eq, Show)

$(deriveJSON requestOptions ''ApiRequest)
