module Network.AuthorizeNet.Api where

import Control.Lens
import Data.Aeson

import qualified Data.ByteString.Lazy as BSL

import Network.AuthorizeNet.Request
import Network.AuthorizeNet.Response
import Network.AuthorizeNet.Types

import Network.Wreq

-- | The sandbox endpoint for Authorize.NET
sandboxApiConfig :: ApiConfig
sandboxApiConfig = ApiConfig {
  apiConfig_baseUrl = "https://apitest.authorize.net/xml/v1/request.api"
  }

-- | The production endpoint for Authorize.NET
productionApiConfig :: ApiConfig
productionApiConfig = ApiConfig {
  apiConfig_baseUrl = "https://api.authorize.net/xml/v1/request.api"
  }

makeRequest :: ApiConfig -> MerchantAuthentication -> ApiRequest -> IO (Either String ApiResponse)
makeRequest apiConfig merchantAuthentication request = do
  response <- post (apiConfig_baseUrl apiConfig) $ toJSON request
  let mBsl = response ^? responseBody
  case mBsl of
    Nothing -> return $ Left "No response in body"
    Just bsl -> return $ decodeRequestResponse request bsl
