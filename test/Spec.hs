import Test.Network.AuthorizeNet.Api (authorizeNetTests)

import Test.Tasty

main :: IO ()
main = defaultMain authorizeNetTests
