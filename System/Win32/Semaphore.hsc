#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Semaphore
-- Copyright   :  (c) Sam Derbyshire, 2022
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Sam Derbyshire
-- Stability   :  provisional
-- Portability :  portable
--
-- Windows Semaphore objects and operations
--
-----------------------------------------------------------------------------

module System.Win32.Semaphore
    ( -- * Semaphores
      Semaphore(..)

      -- * Access modes
    , AccessMode
    , sEMAPHORE_ALL_ACCESS
    , sEMAPHORE_MODIFY_STATE

      -- * Managing semaphores
    , createSemaphore
    , openSemaphore
    , releaseSemaphore
    ) where

import System.Win32.File
import System.Win32.Types

import Data.Maybe (fromMaybe)
import Foreign hiding (void)
import Foreign.C (withCAString)

##include "windows_cconv.h"

#include <windows.h>

----------------------------------------------------------------
-- Semaphore access modes
----------------------------------------------------------------

#{enum AccessMode,
    , sEMAPHORE_ALL_ACCESS   = SEMAPHORE_ALL_ACCESS
    , sEMAPHORE_MODIFY_STATE = SEMAPHORE_MODIFY_STATE
    }

----------------------------------------------------------------
-- Semaphores
----------------------------------------------------------------

-- | A Windows semaphore.
--
-- To obtain a 'Semaphore', use 'createSemaphore' to create a new one,
-- or 'openSemaphore' to open an existing one.
--
-- To wait on a semaphore, use 'System.Win32.Event.waitForSingleObject'.
--
-- To release resources on a semaphore, use 'releaseSemaphore'.
--
-- To free a semaphore, use 'System.Win32.File.closeHandle'.
-- The semaphore object is destroyed when its last handle has been closed.
-- Closing the handle does not affect the semaphore count; therefore, be sure to call
-- 'releaseSemaphore' before closing the handle or before the process terminates.
-- Otherwise, pending wait operations will either time out or continue indefinitely,
-- depending on whether a time-out value has been specified.
newtype Semaphore = Semaphore { semaphoreHandle :: HANDLE }

-- | Open a 'Semaphore' with the given name, or create a new semaphore
-- if no such semaphore exists, with initial count @i@ and maximum count @m@.
--
-- The counts must satisfy @i >= 0@, @m > 0@ and @i <= m@.
--
-- The returned 'Bool' is 'True' if the function found an existing semaphore
-- with the given name, in which case a handle to that semaphore is returned
-- and the counts are ignored.
--
-- Use 'openSemaphore' if you don't want to create a new semaphore.
createSemaphore :: Maybe SECURITY_ATTRIBUTES
                -> LONG         -- ^ initial count @i@ with @0 <= i <= m@
                -> LONG         -- ^ maximum count @m > 0@
                -> Maybe String -- ^ (optional) semaphore name
                                -- (case-sensitive, limited to MAX_PATH characters)
                -> IO (Semaphore, Bool)
createSemaphore mb_sec initial_count max_count mb_name =
  maybeWith with mb_sec $ \ c_sec -> do
  maybeWith withCAString mb_name $ \ c_name -> do
  handle <- c_CreateSemaphore c_sec initial_count max_count c_name
  err_code <- getLastError
  already_exists <-
    case err_code of
      (# const ERROR_INVALID_HANDLE) ->
        errorWin $ "createSemaphore: semaphore name '"
                ++ fromMaybe "" mb_name
                ++ "' matches non-semaphore"
      (# const ERROR_ALREADY_EXISTS) ->
        return True
      _                              ->
        return False
  if handle == nullPtr
  then errorWin "createSemaphore"
  else return (Semaphore handle, already_exists)

foreign import WINDOWS_CCONV unsafe "windows.h CreateSemaphoreA"
  c_CreateSemaphore :: LPSECURITY_ATTRIBUTES -> LONG -> LONG -> LPCSTR -> IO HANDLE

-- | Open an existing 'Semaphore'.
openSemaphore :: AccessMode -- ^ desired access mode
              -> Bool       -- ^ should child processes inherit the handle?
              -> String     -- ^ name of the semaphore to open (case-sensitive)
              -> IO Semaphore
openSemaphore amode inherit name =
  withTString name $ \c_name -> do
    handle <- failIfNull ("openSemaphore: '" ++ name ++ "'") $
              c_OpenSemaphore (fromIntegral amode) inherit c_name
    return (Semaphore handle)

foreign import WINDOWS_CCONV unsafe "windows.h OpenSemaphoreW"
  c_OpenSemaphore :: DWORD -> BOOL -> LPCWSTR -> IO HANDLE

-- | Increase the count of the 'Semaphore' by the specified amount.
--
-- Returns the count of the semaphore before the increase.
--
-- Throws an error if the count would exceeded the maximum count
-- of the semaphore.
releaseSemaphore :: Semaphore -> LONG -> IO LONG
releaseSemaphore (Semaphore handle) count =
  with 0 $ \ ptr_prevCount -> do
  failIfFalse_ "releaseSemaphore" $ c_ReleaseSemaphore handle count ptr_prevCount
  peek ptr_prevCount

foreign import WINDOWS_CCONV unsafe "windows.h ReleaseSemaphore"
  c_ReleaseSemaphore :: HANDLE -> LONG -> Ptr LONG -> IO BOOL

----------------------------------------------------------------
-- End
----------------------------------------------------------------
