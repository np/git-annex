{- git-annex assistant webapp types
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Assistant.WebApp.Types where

import Assistant.Common
import Assistant.Ssh
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.ScanRemotes
import Assistant.TransferQueue
import Assistant.TransferSlots
import Assistant.Alert
import Assistant.Pairing
import Utility.NotificationBroadcaster
import Utility.WebApp
import Logs.Transfer

import Yesod
import Yesod.Static
import Data.Text (Text, pack, unpack)
import Control.Concurrent.STM

publicFiles "static"

mkYesodData "WebApp" $(parseRoutesFile "Assistant/WebApp/routes")

data WebApp = WebApp
	{ threadState :: Maybe ThreadState
	, daemonStatus :: DaemonStatusHandle
	, scanRemotes :: ScanRemoteMap
	, transferQueue :: TransferQueue
	, transferSlots :: TransferSlots
	, secretToken :: Text
	, relDir :: Maybe FilePath
	, getStatic :: Static
	, webAppState :: TMVar WebAppState
	, postFirstRun :: Maybe (IO String)
	}

instance Yesod WebApp where
	{- Require an auth token be set when accessing any (non-static route) -}
	isAuthorized _ _ = checkAuthToken secretToken

	{- Add the auth token to every url generated, except static subsite
         - urls (which can show up in Permission Denied pages). -}
	joinPath = insertAuthToken secretToken excludeStatic
		where
			excludeStatic [] = True
			excludeStatic (p:_) = p /= "static"

	makeSessionBackend = webAppSessionBackend
	jsLoader _ = BottomOfHeadBlocking

instance RenderMessage WebApp FormMessage where
	renderMessage _ _ = defaultFormMessage

type Form x = Html -> MForm WebApp WebApp (FormResult x, Widget)

data WebAppState = WebAppState
	{ showIntro :: Bool -- should the into message be displayed?
	, otherRepos :: [(String, String)] -- name and path to other repos
	}

instance PathPiece SshData where
    toPathPiece = pack . show
    fromPathPiece = readish . unpack

instance PathPiece NotificationId where
    toPathPiece = pack . show
    fromPathPiece = readish . unpack

instance PathPiece AlertId where
    toPathPiece = pack . show
    fromPathPiece = readish . unpack

instance PathPiece Transfer where
    toPathPiece = pack . show
    fromPathPiece = readish . unpack

instance PathPiece PairMsg where
    toPathPiece = pack . show
    fromPathPiece = readish . unpack

instance PathPiece SecretReminder where
    toPathPiece = pack . show
    fromPathPiece = readish . unpack

instance PathPiece UUID where
    toPathPiece = pack . show
    fromPathPiece = readish . unpack
