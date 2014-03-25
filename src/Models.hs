{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Models where

import           Network.Xmpp
import           Network.Xmpp.IM
import           Control.Monad.Reader
import qualified Data.Text                 as T

data Configuration = Configuration {
    clientSession :: Session,
    contactList :: ContactList
}

type ContactList = [Contact]

data Contact = Contact {
    contactJid :: Jid,
    name :: T.Text,
    inputName :: String,
    outputName :: String
} deriving (Show)

type XIConfig a = ReaderT Configuration IO a
