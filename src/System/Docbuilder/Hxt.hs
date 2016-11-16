{-# LANGUAGE Arrows #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{- |
Module      : Docbuilder.Hxt
Description : Hxt bindings for manipulating html in documentation 
Copyright   : Plow Technologies LLC

Maintainer  : Scott Murphy


Parsers and Printers for processing html files

| -}


module System.Docbuilder.Hxt where

import qualified Text.XML.HXT.Core as Hxt
import Control.Lens
import Text.Regex.Lens
import Text.Regex.Quote
import Text.Regex.Posix (Regex)
import Data.Monoid ((<>))
import  Text.XML.HXT.Core ((>>>),when) 




-- | Change the reference for all urls to doc/
rebaseDocs :: String -> Hxt.IOSLA (Hxt.XIOState s) a String
rebaseDocs doc = Hxt.readDocument [] doc >>> changeLocalRef >>> Hxt.writeDocumentToString []
  where

  changeLocalRef = Hxt.processTopDown editHref
    where
      editHref = Hxt.processAttrl ( Hxt.changeAttrValue (addPrefix "docs/") `when`
                                  Hxt.hasName "href")
                 `when`
                 (Hxt.isElem >>> Hxt.hasName "a")



  addPrefix :: String -> String -> String
  addPrefix prefix str = str ^? regex [r|(\./)(.*)|] . captures . ix 1 & maybe str addPrefix'
    where
    addPrefix' = (prefix <> )                                     
