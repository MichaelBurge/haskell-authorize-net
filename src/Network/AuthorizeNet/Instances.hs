module Network.AuthorizeNet.Instances where

import Text.XML.HaXml
import Text.XML.HaXml.Schema.Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as XML

import qualified Data.Text as T


class SchemaType a => XmlParsable a where
  xmlParsableName :: a -> String

instance SchemaType T.Text where
  parseSchemaType s = do
    (XML.XsdString x) <- parseSchemaType s
    return $ T.pack x
  schemaTypeToXML s text = schemaTypeToXML s (XML.XsdString $ T.unpack text)
