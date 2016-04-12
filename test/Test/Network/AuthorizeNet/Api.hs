{-# LANGUAGE QuasiQuotes, OverloadedStrings, ScopedTypeVariables #-}

module Test.Network.AuthorizeNet.Api (authorizeNetTests) where

import Test.Network.AuthorizeNet.Request
import Test.Network.AuthorizeNet.Response

import Test.Tasty

authorizeNetTests :: TestTree
authorizeNetTests = testGroup "Authorize.NET tests" [
  requestTests,
  responseTests
  ]
