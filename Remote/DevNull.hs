{- The /dev/null remote
 -
 - Copyright 2012 Nicolas Pouillard <np@nicolaspouillard.fr>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.DevNull (remote) where

import Common.Annex
import Types.Remote
import qualified Git
import qualified Git.Construct
import Config
import Annex (devnullUUID)

remotename :: String
remotename = "/dev/null"

remote :: RemoteType
remote = RemoteType {
	typename = remotename,
	enumerate = list,
	generate = gen,
	setup = error "not supported"
}

-- There is only one /dev/null remote, and it always exists.
list :: Annex [Git.Repo]
list = (:[]) <$> liftIO (Git.Construct.remoteNamed remotename Git.Construct.fromUnknown)

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex Remote
gen r _ _ = do
	return Remote {
		uuid = devnullUUID,
		cost = expensiveRemoteCost, -- it is actually very cheap
		name = Git.repoDescribe r,
		storeKey = uploadKey,
		retrieveKeyFile = downloadKey,
		retrieveKeyFileCheap = downloadKeyCheap,
		removeKey = dropKey,
		hasKey = checkKey,
		hasKeyCheap = True,
		whereisKey = Nothing,
		config = Nothing,
		path = Nothing,
		repo = r,
		remotetype = remote
	}

downloadKey :: Key -> AssociatedFile -> FilePath -> Annex Bool
downloadKey k _ p = downloadKeyCheap k p

downloadKeyCheap :: Key -> FilePath -> Annex Bool
downloadKeyCheap _ _ =
  warning ("getting from " ++ remotename ++ " makes no sense") >> return False

-- Uploading to /dev/null is easy, cheap but gives you no way to get back the
-- data :) However, location tracking will remember what is in /dev/null
uploadKey :: Key -> AssociatedFile -> Annex Bool
uploadKey _ _ = return True

dropKey :: Key -> Annex Bool
dropKey _ = return True

checkKey :: Key -> Annex (Either String Bool)
checkKey _ = return $ Right False
