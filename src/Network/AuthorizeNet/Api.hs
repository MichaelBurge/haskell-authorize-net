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
stripBom bsl = case BSL.splitAt 1 bsl of
  (bom, xs) | bom == BSL.pack [ 0xFF ] -> xs
--(bom, xs) | bom == BSL.pack [ 0xEF, 0xBB, 0xBF] -> xs
  _ -> bsl

-- | Makes an Authorize.NET request, hopefully returning an ApiResponse. If an error occurs, returns the raw response body and the Aeson parse error.
makeRequest :: ApiConfig -> ApiRequest -> IO (Either (BSL.ByteString, String) ApiResponse)
makeRequest apiConfig request = do
  response <- post (apiConfig_baseUrl apiConfig) $ toJSON request
  let mBsl = response ^? responseBody
  case mBsl of
    Nothing -> return $ Left (BSL.empty, "No response in body")
    Just bsl ->
      let strippedBsl = stripBom bsl
      in case decodeRequestResponse request strippedBsl of
        Left e -> return $ Left (strippedBsl, e)
        Right x -> return $ Right x
  
