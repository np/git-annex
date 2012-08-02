{- git-annex assistant git merge thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Merger (
	mergeThread,
	manualPull,
) where

import Assistant.Common
import Assistant.ThreadedMonad
import Utility.DirWatcher
import Utility.Types.DirWatcher
import qualified Annex.Branch
import qualified Git
import qualified Git.Command
import qualified Git.Merge
import qualified Git.Branch
import qualified Command.Sync
import qualified Remote

thisThread :: ThreadName
thisThread = "Merger"

{- This thread watches for changes to .git/refs/heads/synced/,
 - which indicate incoming pushes. It merges those pushes into the
 - currently checked out branch. -}
mergeThread :: ThreadState -> IO ()
mergeThread st = do
	g <- runThreadState st $ fromRepo id
	let dir = Git.localGitDir g </> "refs" </> "heads" </> "synced"
	createDirectoryIfMissing True dir
	let hook a = Just $ runHandler g a
	let hooks = mkWatchHooks
		{ addHook = hook onAdd
		, errHook = hook onErr
		}
	void $ watchDir dir (const False) hooks id
	debug thisThread ["watching", dir]

type Handler = Git.Repo -> FilePath -> Maybe FileStatus -> IO ()

{- Runs an action handler.
 -
 - Exceptions are ignored, otherwise a whole thread could be crashed.
 -}
runHandler :: Git.Repo -> Handler -> FilePath -> Maybe FileStatus -> IO ()
runHandler g handler file filestatus = void $ do
        either print (const noop) =<< tryIO go
        where
                go = handler g file filestatus

{- Called when there's an error with inotify. -}
onErr :: Handler
onErr _ msg _ = error msg

{- Called when a new branch ref is written.
 -
 - This relies on git's atomic method of updating branch ref files,
 - which is to first write the new file to .lock, and then rename it
 - over the old file. So, ignore .lock files, and the rename ensures
 - the watcher sees a new file being added on each update.
 -
 - At startup, synthetic add events fire, causing this to run, but that's
 - ok; it ensures that any changes pushed since the last time the assistant
 - ran are merged in.
 -}
onAdd :: Handler
onAdd g file _
	| ".lock" `isSuffixOf` file = noop
	| otherwise = do
		let changedbranch = Git.Ref $
			"refs" </> "heads" </> takeFileName file
		current <- Git.Branch.current g
		when (Just changedbranch == current) $ do
			liftIO $ debug thisThread
				[ "merging changes into"
				, show current
				]
			void $ mergeBranch changedbranch g

mergeBranch :: Git.Ref -> Git.Repo -> IO Bool
mergeBranch = Git.Merge.mergeNonInteractive . Command.Sync.syncBranch

{- Manually pull from remotes and merge their branches. Called by the pusher
 - when a push fails, which can happen due to a remote not having pushed
 - changes to us. That could be because it doesn't have us as a remote, or
 - because the assistant is not running there, or other reasons. -}
manualPull :: Git.Ref -> [Remote] -> Annex ()
manualPull currentbranch remotes = do
	forM_ remotes $ \r ->
		inRepo $ Git.Command.runBool "fetch" [Param $ Remote.name r]
	Annex.Branch.forceUpdate
	forM_ remotes $ \r ->
		Command.Sync.mergeRemote r currentbranch