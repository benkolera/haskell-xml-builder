{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.XML.Builder where

import           Control.Category     ((.))
import           Control.Monad.Writer (Writer, execWriter, tell)
import           Data.Foldable        (Foldable, traverse_)
import           Data.Function        (flip, ($))
import qualified Data.Map             as M
import           Data.Maybe           (Maybe (Just, Nothing))
import           Data.Semigroup       ((<>))
import           Data.Text            (Text, pack)
import           Text.Show            (Show, show)
import           Text.XML

class ToDocument a where
  toDocument :: a -> Document

class ToNode a where
  toNode :: a -> NodeM

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

textNodeShow :: Show a => Name -> a -> NodeM
textNodeShow n = textNode n . pack . show

textNodesShow :: (Foldable f, Show a) => Name -> f a -> NodeM
textNodesShow n = traverse_ (textNodeShow n)

textNodes :: (Foldable f) => Name -> f Text -> NodeM
textNodes n = traverse_ (textNode n)

subElement :: Element -> NodeM
subElement = tell . (:[]) . NodeElement

subElements :: Foldable f => f Element -> NodeM
subElements = traverse_ subElement

subNode :: ToNode a => (NodeM -> Element) -> a -> NodeM
subNode f = subElement . f . toNode

-- TODO: I don't really like these pluralised names when it is over
-- a foldable. Makes it hard for the user to guess that it could work
-- with a maybe if they are just seeing the function name.
subNodes :: (ToNode a,Foldable t) => (NodeM -> Element) -> t a -> NodeM
subNodes f = traverse_ (subElement . f . toNode)

element :: Name -> NodeM -> Element
element n ns = Element n M.empty (execWriter ns)

nsAttr :: Text -> Text -> Attribute
nsAttr p u = (Name p Nothing (Just "xmlns"),u)

nsAttrXsi :: Attribute
nsAttrXsi = nsAttr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance"

typeAttr :: Text -> Attribute
typeAttr t = ("xsi:type",t)
