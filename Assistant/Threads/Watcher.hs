{- git-annex assistant tree watcher
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Watcher (
	watchThread,
	checkCanWatch,
	needLsof,
	stageSymlink,
	onAddSymlink,
	runHandler,
) where

import Assistant.Common
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.Changes
import Assistant.TransferQueue
import Assistant.Alert
import Logs.Transfer
import Utility.DirWatcher
import Utility.Types.DirWatcher
import qualified Annex
import qualified Annex.Queue
import qualified Git.Command
import qualified Git.UpdateIndex
import qualified Git.HashObject
import qualified Backend
import Annex.Content
import Annex.CatFile
import Git.Types

import Data.Bits.Utils
import qualified Data.ByteString.Lazy as L

thisThread :: ThreadName
thisThread = "Watcher"

checkCanWatch :: Annex ()
checkCanWatch
	| canWatch = 
		unlessM (liftIO (inPath "lsof") <||> Annex.getState Annex.force)
			needLsof
	| otherwise = error "watch mode is not available on this system"

needLsof :: Annex ()
needLsof = error $ unlines
	[ "The lsof command is needed for watch mode to be safe, and is not in PATH."
	, "To override lsof checks to ensure that files are not open for writing"
	, "when added to the annex, you can use --force"
	, "Be warned: This can corrupt data in the annex, and make fsck complain."
	]

watchThread :: ThreadState -> DaemonStatusHandle -> TransferQueue -> ChangeChan -> NamedThread
watchThread st dstatus transferqueue changechan = NamedThread thisThread $ do
	void $ watchDir "." ignored hooks startup
	debug thisThread [ "watching", "."]
	where
		startup = startupScan st dstatus
		hook a = Just $ runHandler thisThread st dstatus transferqueue changechan a
		hooks = mkWatchHooks
			{ addHook = hook onAdd
			, delHook = hook onDel
			, addSymlinkHook = hook onAddSymlink
			, delDirHook = hook onDelDir
			, errHook = hook onErr
			}

{- Initial scartup scan. The action should return once the scan is complete. -}
startupScan :: ThreadState -> DaemonStatusHandle -> IO a -> IO a
startupScan st dstatus scanner = do
	runThreadState st $ showAction "scanning"
	alertWhile' dstatus startupScanAlert $ do
		r <- scanner

		-- Notice any files that were deleted before
		-- watching was started.
		runThreadState st $ do
			inRepo $ Git.Command.run "add" [Param "--update"]
			showAction "started"
		
		modifyDaemonStatus_ dstatus $ \s -> s { scanComplete = True }

		return (True, r)

ignored :: FilePath -> Bool
ignored = ig . takeFileName
	where
	ig ".git" = True
	ig ".gitignore" = True
	ig ".gitattributes" = True
	ig _ = False

type Handler = ThreadName -> FilePath -> Maybe FileStatus -> DaemonStatusHandle -> TransferQueue -> Annex (Maybe Change)

{- Runs an action handler, inside the Annex monad, and if there was a
 - change, adds it to the ChangeChan.
 -
 - Exceptions are ignored, otherwise a whole watcher thread could be crashed.
 -}
runHandler :: ThreadName -> ThreadState -> DaemonStatusHandle -> TransferQueue -> ChangeChan -> Handler -> FilePath -> Maybe FileStatus -> IO ()
runHandler threadname st dstatus transferqueue changechan handler file filestatus = void $ do
	r <- tryIO go
	case r of
		Left e -> print e
		Right Nothing -> noop
		Right (Just change) -> recordChange changechan change
	where
		go = runThreadState st $ handler threadname file filestatus dstatus transferqueue

onAdd :: Handler
onAdd _ file filestatus _ _
	| maybe False isRegularFile filestatus = pendingAddChange file
	| otherwise = noChange
	where

{- A symlink might be an arbitrary symlink, which is just added.
 - Or, if it is a git-annex symlink, ensure it points to the content
 - before adding it.
 -}
onAddSymlink :: Handler
onAddSymlink threadname file filestatus dstatus transferqueue = go =<< Backend.lookupFile file
	where
		go (Just (key, _)) = do
			link <- calcGitLink file key
			ifM ((==) link <$> liftIO (readSymbolicLink file))
				( do
					s <- liftIO $ getDaemonStatus dstatus
					checkcontent key s
					ensurestaged link s
				, do
					liftIO $ debug threadname ["fix symlink", file]
					liftIO $ removeFile file
					liftIO $ createSymbolicLink link file
					addlink link
				)
		go Nothing = do -- other symlink
			link <- liftIO (readSymbolicLink file)
			ensurestaged link =<< liftIO (getDaemonStatus dstatus)

		{- This is often called on symlinks that are already
		 - staged correctly. A symlink may have been deleted
		 - and being re-added, or added when the watcher was
		 - not running. So they're normally restaged to make sure.
		 -
		 - As an optimisation, during the status scan, avoid
		 - restaging everything. Only links that were created since
		 - the last time the daemon was running are staged.
		 - (If the daemon has never ran before, avoid staging
		 - links too.)
		 -}
		ensurestaged link daemonstatus
			| scanComplete daemonstatus = addlink link
			| otherwise = case filestatus of
				Just s
					| not (afterLastDaemonRun (statusChangeTime s) daemonstatus) -> noChange
				_ -> addlink link

		{- For speed, tries to reuse the existing blob for
		 - the symlink target. -}
		addlink link = do
			liftIO $ debug threadname ["add symlink", file]
			v <- catObjectDetails $ Ref $ ':':file
			case v of
				Just (currlink, sha)
					| s2w8 link == L.unpack currlink ->
						stageSymlink file sha
				_ -> do
					sha <- inRepo $
						Git.HashObject.hashObject BlobObject link
					stageSymlink file sha
			madeChange file LinkChange

		{- When a new link appears, after the startup scan,
		 - try to get the key's content. -}
		checkcontent key daemonstatus
			| scanComplete daemonstatus = unlessM (inAnnex key) $
				queueTransfers Next transferqueue dstatus
					key (Just file) Download
			| otherwise = noop

onDel :: Handler
onDel threadname file _ _dstatus _ = do
	liftIO $ debug threadname ["file deleted", file]
	Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.unstageFile file)
	madeChange file RmChange

{- A directory has been deleted, or moved, so tell git to remove anything
 - that was inside it from its cache. Since it could reappear at any time,
 - use --cached to only delete it from the index. 
 -
 - Note: This could use unstageFile, but would need to run another git
 - command to get the recursive list of files in the directory, so rm is
 - just as good. -}
onDelDir :: Handler
onDelDir threadname dir _ _dstatus _ = do
	liftIO $ debug threadname ["directory deleted", dir]
	Annex.Queue.addCommand "rm"
		[Params "--quiet -r --cached --ignore-unmatch --"] [dir]
	madeChange dir RmDirChange

{- Called when there's an error with inotify or kqueue. -}
onErr :: Handler
onErr _ msg _ dstatus _ = do
	warning msg
	void $ liftIO $ addAlert dstatus $ warningAlert "watcher" msg
	return Nothing

{- Adds a symlink to the index, without ever accessing the actual symlink
 - on disk. This avoids a race if git add is used, where the symlink is
 - changed to something else immediately after creation.
 -}
stageSymlink :: FilePath -> Sha -> Annex ()
stageSymlink file sha =
	Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.stageSymlink file sha)
