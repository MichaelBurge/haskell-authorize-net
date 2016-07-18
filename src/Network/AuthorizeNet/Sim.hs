{-# LANGUAGE RankNTypes, GADTs, ScopedTypeVariables, OverloadedStrings #-}

module Network.AuthorizeNet.Sim where

import Network.AuthorizeNet.Types

import Control.Monad
import Control.Monad.Trans.Writer.Strict
import Crypto.Hash.Algorithms
import Crypto.MAC.HMAC
import Data.Maybe
import Data.UnixTime

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

class FormField a where
  renderField :: a -> T.Text

instance FormField Int where
  renderField x = T.pack $ show x

instance FormField UnixTime where
  renderField x = T.pack $ show $ utSeconds x

instance FormField Bool where
  renderField True = "TRUE"
  renderField False = "FALSE"

instance FormField T.Text where
  renderField = id

instance FormField TransactionType where
  renderField Transaction_authOnlyTransaction = "AUTH_ONLY"
  renderField Transaction_authCaptureTransaction = "AUTH_CAPTURE"

instance FormField Decimal where
  renderField (Decimal text) = text

data AnyFormField = forall a. FormField a => MkField a

class FormFieldCollection a where
  renderFields :: a -> [(T.Text, AnyFormField)]

data TransactionFingerprint = TransactionFingerprint {
  transactionFingerprint_merchantAuthentication :: MerchantAuthentication,
  transactionFingerprint_transactionId          :: TransactionId,
  transactionFingerprint_timestamp              :: UnixTime,
  transactionFingerprint_priceCents             :: Decimal
  } deriving (Eq, Show)

instance FormField TransactionFingerprint where
  renderField (TransactionFingerprint auth (NumericString b) c d) =
    let a' = merchantAuthentication_name auth
        b' = T.pack $ show b
        c' = T.pack $ show $ utSeconds c
        (Decimal d') = d
        message = T.encodeUtf8 $ T.intercalate "^" $ [ a', b', c', d',"" ]
        key = T.encodeUtf8 $ merchantAuthentication_transactionKey auth
        fingerprint = hmac key message :: HMAC MD5
    in T.pack $ show $ hmacGetDigest $ fingerprint

instance FormField NumericString where
  renderField (NumericString x) = T.pack $ show x

-- | See "Chapter 3: Table 7 - Custom Transaction Fingerprint Code" in http://www.authorize.net/content/dam/authorize/documents/SIM_guide.pdf
instance FormFieldCollection TransactionFingerprint where
  renderFields x = [
    ("x_fp_hash",      MkField x),
    ("x_fp_sequence",  MkField $ transactionFingerprint_transactionId x),
    ("x_fp_timestamp", MkField $ transactionFingerprint_timestamp x)
    ]

-- | See Chapter 5, Table 15, x_version in SIM Guide
data TransactionVersion = Version_3_0
                        | Version_3_1
                        deriving (Eq, Show)

instance FormField TransactionVersion where
  renderField Version_3_0 = "3.0"
  renderField Version_3_1 = "3.1"

data PaymentMethod = PaymentMethod_CreditCard
                   | PaymentMethod_ECheck
                   deriving (Eq, Show)

instance FormField PaymentMethod where
  renderField PaymentMethod_CreditCard = "CC"
  renderField PaymentMethod_ECheck = "ECHECK"

data RequestSecureHostedPaymentForm = RequestSecureHostedPaymentForm {
  requestSecureHostedPaymentForm_merchantAuthentication :: MerchantAuthentication,
  requestSecureHostedPaymentForm_transactionId          :: TransactionId,
  requestSecureHostedPaymentForm_timestamp              :: UnixTime,
  requestSecureHostedPaymentForm_transactionType        :: TransactionType,
  requestSecureHostedPaymentForm_amount                 :: Decimal,
  -- | Either Nothing(for no relay response), or a relay url and whether it should be hit even in the case of errors.
  requestSecureHostedPaymentForm_relayResponseUrl       :: Maybe (T.Text, Bool),
  requestSecureHostedPaymentForm_lineItems              :: [ LineItem ],
  
  requestSecureHostedPaymentForm_version                :: Maybe TransactionVersion,
  requestSecureHostedPaymentForm_method                 :: Maybe PaymentMethod,
  requestSecureHostedPaymentForm_testRequest            :: Maybe Bool,
  requestSecureHostedPaymentForm_duplicateWindow        :: Maybe Int,
  requestSecureHostedPaymentForm_customerIp             :: Maybe T.Text
  } deriving (Eq, Show)

data ResponseCode = Response_Approved
                  | Response_Declined
                  | Response_Error
                  | Response_HeldForReview
                  deriving (Eq, Show)

-- | Interpret these using this Authorize.NET tool: http://developer.authorize.net/api/reference/responseCodes.html
newtype ResponseReasonCode = ResponseReasonCode Int deriving (Eq, Show)

data ResponseSecureHostedPaymentForm = ResponseSecureHostedPaymentForm {
  responseSecureHostedPaymentForm_responseCode           :: ResponseCode,
  responseSecureHostedPaymentForm_responseReasonCode     :: ResponseReasonCode,
  responseSecureHostedPaymentForm_responseReasonText     :: T.Text,
  responseSecureHostedPaymentForm_authCode               :: Maybe AuthCode,
  responseSecureHostedPaymentForm_avsCode                :: Maybe AvsCode,
  responseSecureHostedPaymentForm_transactionId          :: Maybe TransactionId,
  responseSecureHostedPaymentForm_invoiceNumber          :: Maybe InvoiceNumber,
  responseSecureHostedPaymentForm_description            :: Maybe T.Text,
  responseSecureHostedPaymentForm_amount                 :: Maybe Decimal,
  responseSecureHostedPaymentForm_method                 :: Maybe PaymentMethod,
  responseSecureHostedPaymentForm_type                   :: Maybe TransactionType,
  responseSecureHostedPaymentForm_accountNumber          :: Maybe T.Text,
  responseSecureHostedPaymentForm_cardType               :: Maybe T.Text,
  responseSecureHostedPaymentForm_splitTenderId          :: Maybe T.Text,
  responseSecureHostedPaymentForm_prepaidRequestedAmount :: Maybe Decimal,
  responseSecureHostedPaymentForm_prepaidBalanceOnCard   :: Maybe Decimal,
  responseSecureHostedPaymentForm_customerId             :: Maybe MerchantCustomerId,
  responseSecureHostedPaymentForm_firstName              :: Maybe T.Text,
  responseSecureHostedPaymentForm_lastName               :: Maybe T.Text,
  responseSecureHostedPaymentForm_company                :: Maybe T.Text,
  responseSecureHostedPaymentForm_address                :: Maybe T.Text,
  responseSecureHostedPaymentForm_city                   :: Maybe T.Text,
  responseSecureHostedPaymentForm_state                  :: Maybe T.Text,
  responseSecureHostedPaymentForm_zip                    :: Maybe T.Text,
  responseSecureHostedPaymentForm_country                :: Maybe T.Text,
  responseSecureHostedPaymentForm_phone                  :: Maybe T.Text,
  responseSecureHostedPaymentForm_fax                    :: Maybe T.Text,
  responseSecureHostedPaymentForm_email                  :: Maybe T.Text,
  responseSecureHostedPaymentForm_shipToFirstName        :: Maybe T.Text,
  responseSecureHostedPaymentForm_shipToLastName         :: Maybe T.Text,
  responseSecureHostedPaymentForm_shipToCompany          :: Maybe T.Text,
  responseSecureHostedPaymentForm_shipToAddress          :: Maybe T.Text,
  responseSecureHostedPaymentForm_shipToCity             :: Maybe T.Text,
  responseSecureHostedPaymentForm_shipToState            :: Maybe T.Text,
  responseSecureHostedPaymentForm_shipToZip              :: Maybe T.Text,
  responseSecureHostedPaymentForm_shipToCountry          :: Maybe T.Text,
  responseSecureHostedPaymentForm_tax                    :: Maybe Decimal,
  responseSecureHostedPaymentForm_duty                   :: Maybe Decimal,
  responseSecureHostedPaymentForm_freight                :: Maybe Decimal,
  responseSecureHostedPaymentForm_taxExempt              :: Maybe Bool,
  responseSecureHostedPaymentForm_purchaseOrderNumber    :: Maybe Int,
  responseSecureHostedPaymentForm_md5Hash                :: Maybe T.Text,
  responseSecureHostedPaymentForm_cvv2ResponseCode       :: Maybe T.Text,
  responseSecureHostedPaymentForm_cavvResponseCode       :: Maybe T.Text
  } deriving (Eq, Show)

-- See Chapter 5, Table 16 for a list of restrictions on the line item fields
instance FormField LineItem where
  renderField x =
    let itemId' = T.take 31 $ lineItem_itemId x
        name' = T.take 31 $ lineItem_name x
        desc' = T.take 255 $ fromMaybe "" $ lineItem_description x
        (Decimal quant') = lineItem_quantity x
        (Decimal unitPrice') = lineItem_unitPrice x
        taxable' = renderField $ fromMaybe False $ lineItem_taxable x
    in T.intercalate "<|>" [ itemId', name', desc', quant', unitPrice', taxable' ]

field :: forall a m b. (Monad m, FormField b) => a -> b -> WriterT [(a,AnyFormField)] m ()
field x y = tell $ pure (x, MkField y)

mField :: forall a m b. (Monad m, FormField b) => a -> Maybe b -> WriterT [(a,AnyFormField)] m ()
mField _ Nothing = return ()
mField x (Just y) = tell $ pure (x, MkField y)

instance FormFieldCollection RequestSecureHostedPaymentForm where
  renderFields x = execWriter $ do
    let merchantAuthentication = requestSecureHostedPaymentForm_merchantAuthentication x
        transactionFingerprint = TransactionFingerprint {
          transactionFingerprint_merchantAuthentication = merchantAuthentication,
          transactionFingerprint_transactionId = requestSecureHostedPaymentForm_transactionId x,
          transactionFingerprint_timestamp = requestSecureHostedPaymentForm_timestamp x,
          transactionFingerprint_priceCents = requestSecureHostedPaymentForm_amount x
          }
    tell $ renderFields transactionFingerprint
      
    field  "x_login"            $ merchantAuthentication_name merchantAuthentication
    field  "x_type"             $ requestSecureHostedPaymentForm_transactionType x
    field  "x_amount"           $ requestSecureHostedPaymentForm_amount x
    field  "x_show_form"        $ ("PAYMENT_FORM" :: T.Text)
    case requestSecureHostedPaymentForm_relayResponseUrl x of
      Nothing -> do
        field "x_relay_response" False
      Just (relayResponseUrl, alwaysRelay) -> do
        field "x_relay_response" $ True
        field "x_relay_url"      $ relayResponseUrl
        field "x_relay_always"   $ alwaysRelay
    mField "x_version"          $ requestSecureHostedPaymentForm_version x
    mField "x_method"           $ requestSecureHostedPaymentForm_method x
    mField "x_test_request"     $ requestSecureHostedPaymentForm_testRequest x
    mField "x_duplicate_window" $ requestSecureHostedPaymentForm_duplicateWindow x
    forM_ (requestSecureHostedPaymentForm_lineItems x) $ \lineItem -> do
      field "x_line_item" lineItem
    mField "x_customer_ip" $ requestSecureHostedPaymentForm_customerIp x

type HtmlColor = T.Text
type Url = T.Text
type Font = T.Text

-- | Optional fields you can add to decorate the payment field.
data PaymentFormDecorations = PaymentFormDecorations {
  paymentFormDecorations_returnPolicyUrl        :: Maybe Url,
  paymentFormDecorations_headerHtmlPaymentForm  :: Maybe T.Text,
  paymentFormDecorations_footerHtmlPaymentForm  :: Maybe T.Text,
  paymentFormDecorations_header2HtmlPaymentForm :: Maybe T.Text,
  paymentFormDecorations_footer2HtmlPaymentForm :: Maybe T.Text,
  paymentFormDecorations_colorBackground        :: Maybe HtmlColor,
  paymentFormDecorations_colorLink              :: Maybe HtmlColor,
  paymentFormDecorations_colorText              :: Maybe HtmlColor,
  paymentFormDecorations_logoUrl                :: Maybe Url,
  paymentFormDecorations_backgroundUrl          :: Maybe Url,
  paymentFormDecorations_cancelUrl              :: Maybe Url,
  paymentFormDecorations_cancelUrlText          :: Maybe Url,
  paymentFormDecorations_fontFamily             :: Maybe Font,
  paymentFormDecorations_fontSize               :: Maybe T.Text,
  paymentFormDecorations_sectionHead1Color      :: Maybe HtmlColor,
  paymentFormDecorations_sectionHead1FontFamily :: Maybe Font,
  paymentFormDecorations_sectionHead1FontSize   :: Maybe T.Text,
  paymentFormDecorations_sectionHead1FontBold   :: Maybe Bool,
  paymentFormDecorations_sectionHead1FontItalic :: Maybe Bool
  } deriving (Eq, Show)

defaultPaymentFormDecorations :: PaymentFormDecorations
defaultPaymentFormDecorations = PaymentFormDecorations Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                              
instance FormFieldCollection PaymentFormDecorations where
  renderFields x = execWriter $ do
    mField "x_return_policy_url" $ paymentFormDecorations_returnPolicyUrl x
    mField "x_header_html_payment_form" $ paymentFormDecorations_headerHtmlPaymentForm x
    mField "x_footer_html_payment_form" $ paymentFormDecorations_footerHtmlPaymentForm x
    mField "x_header2_html_payment_form" $ paymentFormDecorations_header2HtmlPaymentForm x
    mField "x_footer2_html_payment_form" $ paymentFormDecorations_footer2HtmlPaymentForm x
    mField "x_color_background" $ paymentFormDecorations_colorBackground x
    mField "x_color_link" $ paymentFormDecorations_colorLink x     
    mField "x_color_text" $ paymentFormDecorations_colorText x           
    mField "x_logo_url" $ paymentFormDecorations_logoUrl x           
    mField "x_background_url" $ paymentFormDecorations_backgroundUrl x
    mField "x_cancel_url" $ paymentFormDecorations_cancelUrl x       
    mField "x_cancel_url_text" $ paymentFormDecorations_cancelUrlText x
    mField "x_font_family" $ paymentFormDecorations_fontFamily x       
    mField "x_font_size" $ paymentFormDecorations_fontSize x          
    mField "x_sectionhead1_color_text" $ paymentFormDecorations_sectionHead1Color x
    mField "x_sectionhead1_font_family" $ paymentFormDecorations_sectionHead1FontFamily x
    mField "x_sectionhead1_font_size" $ paymentFormDecorations_sectionHead1FontSize x
    mField "x_sectionhead1_font_bold" $ paymentFormDecorations_sectionHead1FontBold x
    mField "x_sectionhead1_font_italic" $ paymentFormDecorations_sectionHead1FontItalic x

runRenderFields :: [(T.Text, AnyFormField)] -> [(T.Text, T.Text)]
runRenderFields xs = map (\(a,MkField b) -> (a, renderField b)) xs
