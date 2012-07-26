{- git-annex assistant thread to scan remotes to find needed transfers
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.TransferScanner where

import Assistant.Common
import Assistant.ScanRemotes
import Assistant.TransferQueue
import Assistant.ThreadedMonad
import Logs.Transfer
import Logs.Location
import qualified Remote
import Utility.ThreadScheduler
import qualified Git.LsFiles as LsFiles
import Command
import Annex.Content

thisThread :: ThreadName
thisThread = "TransferScanner"

{- This thread waits until a remote needs to be scanned, to find transfers
 - that need to be made, to keep data in sync.
 -}
transferScannerThread :: ThreadState -> ScanRemoteMap -> TransferQueue -> IO ()
transferScannerThread st scanremotes transferqueue = do
	runEvery (Seconds 2) $ do
		r <- getScanRemote scanremotes
		liftIO $ debug thisThread ["starting scan of", show r]
		scan st transferqueue r
		liftIO $ debug thisThread ["finished scan of", show r]
	where

{- This is a naive scan through the git work tree.
 - 
 - The scan is blocked when the transfer queue gets too large. -}
scan :: ThreadState -> TransferQueue -> Remote -> IO ()
scan st transferqueue r = do
	g <- runThreadState st $ fromRepo id
	files <- LsFiles.inRepo [] g
	go files
	where
		go [] = return ()
		go (f:fs) = do
			v <- runThreadState st $ whenAnnexed check f
			case v of
				Nothing -> noop
				Just t -> do
					debug thisThread ["queuing", show t]
					enqueue f t
			go fs
			where
				check _ (key, _) = ifM (inAnnex key)
					( helper key Upload False =<< remotehas key
					, helper key Download True =<< remotehas key
					)
				helper key direction x y
					| x == y = return $
						Just $ Transfer direction u key
					| otherwise = return Nothing
				
		u = Remote.uuid r
		enqueue f t = queueTransferAt smallsize Later transferqueue (Just f) t r
		smallsize = 10

		{- Look directly in remote for the key when it's cheap;
		 - otherwise rely on the location log. -}
		remotehas key
			| Remote.hasKeyCheap r = (==)
				<$> pure (Right True)
				<*> Remote.hasKey r key
			| otherwise = elem
				<$> pure u
				<*> loggedLocations key