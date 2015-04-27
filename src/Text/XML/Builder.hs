{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.XML.Builder where

import           Control.Applicative  (pure)
import           Control.Category     ((.))
import           Control.Monad.Writer (Writer, execWriter, tell)
import           Data.Foldable        (Foldable, toList)
import           Data.Function        (flip, ($))
import           Data.Functor         (fmap)
import qualified Data.Map             as M
import           Data.Maybe           (Maybe (Just, Nothing), maybe)
import           Data.Semigroup       ((<>))
import           Data.Text            (Text)
import           Text.XML

class ToXml a where
  toXml :: a -> Document

type Attribute = (Name,Text)

class Attributable a where
  addAttribute :: a -> Attribute -> a

instance Attributable Element where
  addAttribute = addAttribute'

instance Attributable (a -> Element) where
  addAttribute f a = flip addAttribute' a . f

(!) :: Attributable a => a -> Attribute -> a
(!) = addAttribute

infixl 8 !

addAttribute' :: Element -> (Name, Text) -> Element
addAttribute' e a = e
  { elementAttributes = (elementAttributes e) <> (M.fromList [a]) }

type NodeM = Writer [Node] ()

soapDocument :: NodeM -> Document
soapDocument b = Document (Prologue [] Nothing []) envelope []
  where
    envelope = Element "soapenv:Envelope" envAttrs [header,body]
    envAttrs = M.fromList [("xmlns:soapenv","http://schemas.xmlsoap.org/soap/envelope/")]
    header   = NodeElement (Element "soapenv:Header" M.empty [])
    body     = NodeElement (Element "soapenv:Body" M.empty (execWriter b))

document :: Element -> Document
document e = Document (Prologue [] Nothing []) e []

textNode :: Name -> Text -> NodeM
textNode n t = tell $ [NodeElement (Element n M.empty [NodeContent t])]

textNodeOpt :: Name -> Maybe Text -> NodeM
textNodeOpt n = maybe (pure ()) (textNode n)

subNode :: Element -> NodeM
subNode = subNodes . (:[])

subNodes :: Foldable t => t Element -> NodeM
subNodes = tell . fmap NodeElement . toList

subNodeOpt :: Maybe Element -> NodeM
subNodeOpt = maybe (pure ()) (subNode)

element :: Name -> NodeM -> Element
element n ns = Element n M.empty (execWriter ns)

nsAttr :: Text -> Text -> Attribute
nsAttr p u = (Name p Nothing (Just "xmlns"),u)

nsAttrXsi :: Attribute
nsAttrXsi = nsAttr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance"

typeAttr :: Text -> Attribute
typeAttr t = ("xsi:type",t)
