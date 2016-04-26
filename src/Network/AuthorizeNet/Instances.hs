{-# LANGUAGE TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}

module Network.AuthorizeNet.Instances where

import Network.AuthorizeNet.TH
import Network.AuthorizeNet.Types

import Language.Haskell.TH.Lift

import Text.XML.HaXml
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as XML

import qualified Data.Text as T

import qualified Text.Parse as Parse

instance SchemaType T.Text where
  parseSchemaType s = do
    e <- Schema.element [s]
    case e of
      Elem _ _ [] -> return $ T.pack ""
      _ -> commit $ interior e $ parseSimpleType
  schemaTypeToXML s text = toXMLElement s [] [toXMLText (simpleTypeText text)]

instance SimpleType T.Text where
  acceptingParser = fmap T.pack (Parse.many Parse.next)
  simpleTypeText text = T.unpack text

$(deriveXml ''NumericString)
$(deriveXml ''MerchantAuthentication)
$(deriveXml ''CustomerType)
$(deriveXml ''NameAndAddress)
$(deriveXml ''CardArt)
$(deriveXml ''CreditCard)
$(deriveXml ''CreditCardMasked)
$(deriveXml ''Decimal)
$(deriveXml ''CustomerAddress)
$(deriveXml ''DriversLicense)
$(deriveXml ''BankAccountType)
$(deriveXml ''EcheckType)
$(deriveXml ''TransactionType)
$(deriveXml ''BankAccount)
$(deriveXml ''BankAccountMasked)
$(deriveXml ''CreditCardTrack)
$(deriveXml ''EncryptedTrackData)
$(deriveXml ''PayPal)
$(deriveXml ''OpaqueData)
$(deriveXml ''PaymentEmv)
$(deriveXml ''Payment)
$(deriveXml ''TokenMasked)
$(deriveXml ''PaymentMasked)
$(deriveXml ''PaymentProfile)
$(deriveXml ''CustomerPaymentProfile)
$(deriveXml ''CustomerPaymentProfileEx)
$(deriveXml ''CustomerPaymentProfileMasked)
$(deriveXml ''CustomerPaymentProfileSearchType)
$(deriveXml ''CustomerPaymentProfileOrderFieldEnum)
$(deriveXml ''CustomerPaymentProfileSorting)
$(deriveXml ''Paging)
$(deriveXml ''CustomerPaymentProfileListItem)
$(deriveXml ''ArrayOfCustomerPaymentProfileListItem)
$(deriveXml ''CustomerProfileBase)
$(deriveXml ''CustomerProfile)
$(deriveXml ''CustomerProfileEx)
$(deriveXml ''CustomerAddressEx)
$(deriveXml ''CustomerProfileMasked)
$(deriveXml ''ValidationMode)
$(deriveXml ''CustomerProfilePayment)
$(deriveXml ''Solution)
$(deriveXml ''Order)
$(deriveXml ''LineItem)
$(deriveXml ''LineItems)
$(deriveXml ''ExtendedAmount)
$(deriveXml ''CustomerData)
$(deriveXml ''CcAuthentication)
$(deriveXml ''TransRetailInfo)
$(deriveXml ''SettingName)
$(deriveXml ''Setting)
$(deriveXml ''ArrayOfSetting)
$(deriveXml ''UserField)
$(deriveXml ''SecureAcceptance)
$(deriveXml ''EmvResponse)
$(deriveXml ''TransactionRequest)
$(deriveXml ''MessageType)
$(deriveXml ''Message)
$(deriveXml ''Messages)
$(deriveXml ''PrePaidCard)
$(deriveXml ''TransactionResponse_message)
$(deriveXml ''TransactionResponse_error)
$(deriveXml ''TransactionResponse_splitTenderPayment)
$(deriveXml ''ANetApiResponse)
