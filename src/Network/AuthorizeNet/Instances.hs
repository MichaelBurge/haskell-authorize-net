module Network.AuthorizeNet.Instances where

import Text.XML.HaXml
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as XML

import qualified Data.Text as T

import qualified Text.Parse as Parse

class SchemaType a => XmlParsable a where
  xmlParsableName :: a -> String

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
