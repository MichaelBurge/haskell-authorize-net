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
  getCustomerProfileResponse_subscriptionIds :: Maybe [SubscriptionId]
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''GetCustomerProfileResponse)
