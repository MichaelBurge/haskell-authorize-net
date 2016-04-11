{-# LANGUAGE TemplateHaskell #-}

module Network.AuthorizeNet.Response where

import Data.Aeson.TH

import qualified Data.Text as T

import Network.AuthorizeNet.Api
import Network.AuthorizeNet.TH

-- | The API responses are documented at http://developer.authorize.net/api/reference/index.html
data ApiResponse = AuthenticateTest {
  authenticateTest_refId        :: Maybe T.Text,
  authenticateTest_messages     :: Messages,
  authenticateTest_sessionToken :: Maybe T.Text
  } deriving (Eq, Show)
                   
$(deriveJSON requestOptions ''ApiResponse)

mkAuthenticateTest :: Messages -> ApiResponse
mkAuthenticateTest messages = AuthenticateTest Nothing messages Nothing
