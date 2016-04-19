{-# LANGUAGE OverloadedStrings #-}

module Network.AuthorizeNet.Api where

import Control.Lens
import Data.Aeson
import Data.Char

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Network.AuthorizeNet.Request
import Network.AuthorizeNet.Response
import Network.AuthorizeNet.Types

import Network.Wreq

-- | The sandbox endpoint for Authorize.NET
sandboxApiConfig :: ApiConfig
sandboxApiConfig = ApiConfig {
  apiConfig_baseUrl          = "https://apitest.authorize.net/xml/v1/request.api",
  apiConfig_hostedProfileUrl = "https://test.authorize.net/profile/manage"
  }

-- | The production endpoint for Authorize.NET
productionApiConfig :: ApiConfig
productionApiConfig = ApiConfig {
  apiConfig_baseUrl          = "https://api.authorize.net/xml/v1/request.api",
  apiConfig_hostedProfileUrl = "https://secure.authorize.net/profile/manage"
  }

stripBom :: BSL.ByteString -> BSL.ByteString
stripBom bsl = case BSL.splitAt 3 bsl of
  (bom, xs) | bom == BSL.pack [ 0xEF, 0xBB, 0xBF] -> xs
  _ -> bsl

data ApiError = ApiError {
  apiError_responseBody :: BSL.ByteString,
  apiError_errorMessage :: String,
  apiError_messages     :: Maybe Messages
  } deriving (Eq, Show)

-- | Makes an Authorize.NET request, hopefully returning an ApiResponse. If an error occurs, returns the raw response body and the Aeson parse error.
makeRequest :: ApiConfig -> ApiRequest -> IO (Either ApiError ApiResponse)
makeRequest apiConfig request = do
  response <- post (apiConfig_baseUrl apiConfig) $ toJSON request
  let mBsl = response ^? responseBody
  case mBsl of
    Nothing -> return $ Left $ ApiError BSL.empty "No response in body" Nothing
    Just bsl ->
      let strippedBsl = stripBom bsl
      in case decodeRequestResponse request strippedBsl of
        Left e -> return $ Left $ ApiError strippedBsl e Nothing
        Right response ->
          let messages = aNetApiResponse_messages $ response_aNetApiResponse response
          in case messages_resultCode messages of
            Message_Ok -> return $ Right response
            Message_Error -> return $ Left $ ApiError bsl "API call returned an error" $ Just messages
