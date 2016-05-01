{-# LANGUAGE QuasiQuotes, OverloadedStrings, ScopedTypeVariables #-}

module Test.Network.AuthorizeNet.Api (authorizeNetTests) where

import qualified Data.ByteString.Lazy

import Network.AuthorizeNet.Api
import Network.AuthorizeNet.Types

import Test.Network.AuthorizeNet.Request
import Test.Network.AuthorizeNet.Response

import Test.Tasty
import Test.Tasty.HUnit

test_stripBom :: Assertion
test_stripBom =
  let expected = "{\"messages\":{\"resultCode\":\"Error\",\"message\":[{\"code\":\"E00003\",\"text\":\"The element 'getHostedProfilePageRequest' in namespace 'AnetApi/xml/v1/schema/AnetApiSchema.xsd' has invalid child element 'customerProfileId' in namespace 'AnetApi/xml/v1/schema/AnetApiSchema.xsd'. List of possible elements expected: 'merchantAuthentication' in namespace 'AnetApi/xml/v1/schema/AnetApiSchema.xsd'.\"}]}}"
      actual = "\xef\xbb\xbf{\"messages\":{\"resultCode\":\"Error\",\"message\":[{\"code\":\"E00003\",\"text\":\"The element 'getHostedProfilePageRequest' in namespace 'AnetApi/xml/v1/schema/AnetApiSchema.xsd' has invalid child element 'customerProfileId' in namespace 'AnetApi/xml/v1/schema/AnetApiSchema.xsd'. List of possible elements expected: 'merchantAuthentication' in namespace 'AnetApi/xml/v1/schema/AnetApiSchema.xsd'.\"}]}}"
  in assertEqual "BOM is stripped" expected $ stripBom actual

test_sanitizeDuplicateRecords :: Assertion
test_sanitizeDuplicateRecords =
  let expected = Just $ SE_DuplicateRecord 40630757
      actual = sanitizeError $ Messages Message_Error $ ArrayOf [
        Message "E00039" "A duplicate record with ID 40630757 already exists."
        ]
  in assertEqual "" expected actual

authorizeNetTests :: TestTree
authorizeNetTests = testGroup "Authorize.NET tests" [
  requestTests,
  responseTests,
  testCase "stripBom" test_stripBom,
  testCase "sanitizeError_duplicateRecords" test_sanitizeDuplicateRecords
  ]
