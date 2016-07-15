{-# LANGUAGE OverloadedStrings,TypeFamilies,RecordWildCards,FlexibleContexts,ScopedTypeVariables #-}

module Network.AuthorizeNet.Api where

import Control.Applicative
import Control.Lens
import Data.Char
import Data.Monoid ((<>))

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Search as BSLS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Text.Regex.Applicative (match, psym, string, RE)

import Network.AuthorizeNet.Instances
import Network.AuthorizeNet.Request
import Network.AuthorizeNet.Response
import Network.AuthorizeNet.Types
import Network.AuthorizeNet.Util

import Network.Wreq


-- | The sandbox endpoint for Authorize.NET
sandboxApiConfig :: ApiConfig
sandboxApiConfig = ApiConfig {
  apiConfig_baseUrl              = "https://apitest.authorize.net/xml/v1/request.api",
  apiConfig_hostedProfileUrlBase = "https://test.authorize.net/profile",
  apiConfig_simPostUrl           = "https://test.authorize.net/gateway/transact.dll"
  }

-- | The production endpoint for Authorize.NET
productionApiConfig :: ApiConfig
productionApiConfig = ApiConfig {
  apiConfig_baseUrl              = "https://api.authorize.net/xml/v1/request.api",
  apiConfig_hostedProfileUrlBase = "https://secure.authorize.net/profile",
  apiConfig_simPostUrl           = "https://secure2.authorize.net/gateway/transact.dll"
  }

getHostedProfileUrl :: ApiConfig -> CimHostedProfileForm -> T.Text
getHostedProfileUrl apiConfig hostedProfileForm =
  let base = apiConfig_hostedProfileUrlBase apiConfig
  in case hostedProfileForm of
    CimHosted_Manage -> base <> "/manage"
    CimHosted_AddPayment -> base <> "/addPayment"
    CimHosted_EditPayment -> base <> "/editPayment"
    CimHosted_AddShipping -> base <> "/addShipping"
    CimHosted_EditShipping -> base <> "/editShipping"

stripBom :: BSL.ByteString -> BSL.ByteString
stripBom bsl = case BSL.splitAt 3 bsl of
  (bom, xs) | bom == BSL.pack [ 0xEF, 0xBB, 0xBF] -> xs
  _ -> bsl

-- | Workaround for https://github.com/bos/aeson/issues/387
stripRepeatedCommas :: BSL.ByteString -> BSL.ByteString
stripRepeatedCommas bsl =
  let f = BSLS.replace ",," ("," :: BSL.ByteString) :: BSL.ByteString -> BSL.ByteString
  in head $ dropWhile (\newBsl -> newBsl /= f newBsl) $ iterate f bsl

-- | There are certain common errors you may want to conveniently pattern-match on.
data SanitizedError = SE_DuplicateRecord {
  duplicateRecord_id :: Integer
  } deriving (Eq, Show)

data ApiError = ApiError {
  apiError_requestBody    :: BSL.ByteString,
  apiError_responseBody   :: BSL.ByteString,
  apiError_errorMessage   :: String,
  apiError_messages       :: Maybe Messages,
  apiError_sanitizedError :: Maybe SanitizedError
  } deriving (Eq, Show)

sanitizeError :: Messages -> Maybe SanitizedError
sanitizeError messages =
    let num :: RE Char Integer
        num = read <$> many (psym isDigit)
        regexes :: RE Char SanitizedError
        regexes = SE_DuplicateRecord <$> (string "A duplicate record with ID " *> num <* " already exists.")
    in case messages_message messages of
      ArrayOf [message] -> match regexes $ T.unpack $ message_text message
      _ -> Nothing

-- | Makes an Authorize.NET request, hopefully returning an ApiResponse. If an error occurs, returns the raw response body and the HaXml parse error
makeRequest :: forall a. (ApiRequest a, ApiResponse (ResponseType a)) => ApiConfig -> a -> IO (Either ApiError (ResponseType a))
makeRequest apiConfig request = do
  let requestBsl = toXml request
  response <- post (apiConfig_baseUrl apiConfig) $ toXml request
  let mResponseBsl = response ^? responseBody
  case mResponseBsl of
    Nothing -> return $ Left $ ApiError requestBsl BSL.empty "No response in body" Nothing Nothing
    Just responseBsl ->
      let strippedBsl = stripBom responseBsl
          errorMessages messages = return $ Left $ ApiError requestBsl strippedBsl "API call returned an error" (Just messages) (sanitizeError messages)
          xmlParseOptions = XmlParseOptions $ Just $ xmlParsableName (undefined :: (ResponseType a))
      in case fromXml strippedBsl :: Either String (ResponseType a) of
        Left _ -> case runParseSchemaTypeWithOptions xmlParseOptions strippedBsl :: Either String ANetApiResponse of
          Left e -> return $ Left $ ApiError requestBsl strippedBsl e Nothing Nothing
          Right apiResponse -> errorMessages $ aNetApiResponse_messages apiResponse
        Right apiResponse ->
          let messages = aNetApiResponse_messages $ aNetApiResponse apiResponse
          in case messages_resultCode messages of
            Message_Ok -> return $ Right apiResponse
            Message_Error -> errorMessages messages
