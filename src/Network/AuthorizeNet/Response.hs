{-# LANGUAGE TemplateHaskell #-}

module Network.AuthorizeNet.Response where

import Data.Aeson.TH

import qualified Data.Text as T

import Network.AuthorizeNet.Api
import Network.AuthorizeNet.TH

-- | The API responses are documented at http://developer.authorize.net/api/reference/index.html
data AuthenticateTest = AuthenticateTest {
  authenticateTest_refId        :: Maybe T.Text,
  authenticateTest_messages     :: Messages,
  authenticateTest_sessionToken :: Maybe T.Text
  } deriving (Eq, Show)

$(deriveJSON dropRecordName ''AuthenticateTest)

data CreateCustomerProfile = CreateCustomerProfile {
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
                   
$(deriveJSON dropRecordName ''CreateCustomerProfile)

mkAuthenticateTest :: Messages -> AuthenticateTest
mkAuthenticateTest messages = AuthenticateTest Nothing messages Nothing
