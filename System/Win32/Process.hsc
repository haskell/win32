-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Process
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32.
--
-----------------------------------------------------------------------------

module System.Win32.Process
  ( sleep       -- :: DWORD -> IO ()
  , iNFINITE    -- :: DWORD
  , spawn       -- :: String -> IO (Handle, Handle, Handle)
  ) where

import System.Win32.Types

#ifdef __GLASGOW_HASKELL__
import GHC.Handle
import System.Posix.Internals( FDType( RegularFile ) )
import Foreign
import Foreign.C
import Control.Monad ( when )
import System.IO
#endif

#include <windows.h>

-- constant to wait for a very long time.
iNFINITE :: DWORD
iNFINITE = #{const INFINITE}

foreign import stdcall unsafe "windows.h Sleep"
  sleep :: DWORD -> IO ()

#ifdef __GLASGOW_HASKELL__
foreign import ccall unsafe "spawnProc.h"
  spawnProc :: CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- | Spawn a Win32 sub-process, to run the command given
-- by the String, returning handles for the child's
--	stdin, stdout, stderr
spawn :: String
	-> IO ( Handle  -- write handle to child's stdin
	      , Handle	-- read handle to child's stdout
	      , Handle  -- read handle to child's stderr
	      )
spawn cmd = 
  withCString cmd $ \ p_cmd ->
   with 0 $ \ p_wIn  ->
    with 0 $ \ p_rOut ->
     with 0 $ \ p_rErr -> do
       rc  <- spawnProc p_cmd p_wIn p_rOut p_rErr
       when (rc /= 0) (ioError (userError ("runProc: unable to spawn " ++ show cmd)))
       wIn <- peek p_wIn
       hIn <- openFd (fromIntegral wIn) (Just RegularFile) 
		     ("<fd " ++ show wIn ++ ">") WriteMode False False
       hSetBuffering hIn NoBuffering
       rOut <- peek p_rOut
       hOut <- openFd (fromIntegral rOut) (Just RegularFile) 
		      ("<fd " ++ show rOut ++ ">") ReadMode True False
       hSetBuffering hOut NoBuffering
       rErr <- peek p_rErr
       hErr <- openFd (fromIntegral rErr) (Just RegularFile) 
		      ("<fd " ++ show rErr ++ ">") ReadMode True False
       hSetBuffering hErr NoBuffering
       return (hIn, hOut, hErr)
#endif /* __GLASGOW_HASKELL__ */
