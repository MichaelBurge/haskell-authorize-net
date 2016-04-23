{-# LANGUAGE OverloadedStrings #-}

module Network.AuthorizeNet.Api where

import Control.Lens
import Data.Aeson
import Data.Char

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Search as BSLS
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

-- | Workaround for https://github.com/bos/aeson/issues/387
stripRepeatedCommas :: BSL.ByteString -> BSL.ByteString
stripRepeatedCommas bsl =
  let f = BSLS.replace ",," ("," :: BSL.ByteString) :: BSL.ByteString -> BSL.ByteString
  in head $ dropWhile (\newBsl -> newBsl /= f newBsl) $ iterate f bsl

data ApiError = ApiError {
  apiError_requestBody  :: BSL.ByteString,
  apiError_responseBody :: BSL.ByteString,
  apiError_errorMessage :: String,
  apiError_messages     :: Maybe Messages
  } deriving (Eq, Show)

-- | Makes an Authorize.NET request, hopefully returning an ApiResponse. If an error occurs, returns the raw response body and the Aeson parse error.
makeRequest :: ApiConfig -> ApiRequest -> IO (Either ApiError ApiResponse)
makeRequest apiConfig request = do
  let requestBsl = stripRepeatedCommas $ encode request
  response <- post (apiConfig_baseUrl apiConfig) $ toJSON request
  let mResponseBsl = response ^? responseBody
  case mResponseBsl of
    Nothing -> return $ Left $ ApiError requestBsl BSL.empty "No response in body" Nothing
    Just responseBsl ->
      let strippedBsl = stripBom responseBsl
      in case decodeRequestResponse request strippedBsl of
        Left e -> return $ Left $ ApiError requestBsl strippedBsl e Nothing
        Right response ->
          let messages = aNetApiResponse_messages $ response_aNetApiResponse response
          in case messages_resultCode messages of
            Message_Ok -> return $ Right response
            Message_Error -> return $ Left $ ApiError requestBsl responseBsl "API call returned an error" $ Just messages
