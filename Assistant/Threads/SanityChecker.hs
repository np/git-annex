{- git-annex assistant sanity checker
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.SanityChecker (
	sanityCheckerThread
) where

import Assistant.Common
import Assistant.DaemonStatus
import Assistant.ThreadedMonad
import Assistant.Changes
import Assistant.Alert
import Assistant.TransferQueue
import qualified Git.LsFiles
import Utility.ThreadScheduler
import qualified Assistant.Threads.Watcher as Watcher

import Data.Time.Clock.POSIX

thisThread :: ThreadName
thisThread = "SanityChecker"

{- This thread wakes up occasionally to make sure the tree is in good shape. -}
sanityCheckerThread :: ThreadState -> DaemonStatusHandle -> TransferQueue -> ChangeChan -> NamedThread
sanityCheckerThread st dstatus transferqueue changechan = thread $ forever $ do
	waitForNextCheck dstatus

	debug thisThread ["starting sanity check"]

	void $ alertWhile dstatus sanityCheckAlert go
	
	debug thisThread ["sanity check complete"]
	where
		thread = NamedThread thisThread
		go = do
			modifyDaemonStatus_ dstatus $ \s -> s
				{ sanityCheckRunning = True }

			now <- getPOSIXTime -- before check started
			r <- catchIO (check st dstatus transferqueue changechan)
				$ \e -> do
					runThreadState st $ warning $ show e
					return False

			modifyDaemonStatus_ dstatus $ \s -> s
				{ sanityCheckRunning = False
				, lastSanityCheck = Just now
				}

			return r

{- Only run one check per day, from the time of the last check. -}
waitForNextCheck :: DaemonStatusHandle -> IO ()
waitForNextCheck dstatus = do
	v <- lastSanityCheck <$> getDaemonStatus dstatus
	now <- getPOSIXTime
	threadDelaySeconds $ Seconds $ calcdelay now v
	where
		calcdelay _ Nothing = oneDay
		calcdelay now (Just lastcheck)
			| lastcheck < now = max oneDay $
				oneDay - truncate (now - lastcheck)
			| otherwise = oneDay

oneDay :: Int
oneDay = 24 * 60 * 60

{- It's important to stay out of the Annex monad as much as possible while
 - running potentially expensive parts of this check, since remaining in it
 - will block the watcher. -}
check :: ThreadState -> DaemonStatusHandle -> TransferQueue -> ChangeChan -> IO Bool
check st dstatus transferqueue changechan = do
	g <- runThreadState st gitRepo
	-- Find old unstaged symlinks, and add them to git.
	(unstaged, cleanup) <- Git.LsFiles.notInRepo False ["."] g
	now <- getPOSIXTime
	forM_ unstaged $ \file -> do
		ms <- catchMaybeIO $ getSymbolicLinkStatus file
		case ms of
			Just s	| toonew (statusChangeTime s) now -> noop
				| isSymbolicLink s ->
					addsymlink file ms
			_ -> noop
	void cleanup
	return True
	where
		toonew timestamp now = now < (realToFrac (timestamp + slop) :: POSIXTime)
		slop = fromIntegral tenMinutes
		insanity msg = do
			runThreadState st $ warning msg
			void $ addAlert dstatus $ sanityCheckFixAlert msg
		addsymlink file s = do
			Watcher.runHandler thisThread st dstatus
				transferqueue changechan
				Watcher.onAddSymlink file s
			insanity $ "found unstaged symlink: " ++ file
