{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils where

import           Data.ByteString           (ByteString)
import           Data.Time                 (ZonedTime)
import           Data.Time.Format          (formatTime)
import           System.Locale             (defaultTimeLocale)
import           Text.Printf               as TP
import           Network.Xmpp
import           Network.Xmpp.IM
import qualified Data.ByteString.Char8     as SC
import qualified Data.Text                 as T

import           Models

prettify :: Contact -> ZonedTime -> ByteString -> ByteString
prettify contact time message = SC.pack (TP.printf "%s at %s: %s\n" (convertContact contact) (convertTime time) (convertMessage message)::String)
    where
        convertContact = T.unpack . jidToText . contactJid
        convertTime = formatTime defaultTimeLocale "%y/%m/%d %H:%M:%S"
        convertMessage = trim . SC.unpack
        trim = unwords . words
