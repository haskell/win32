-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Console
-- Copyright   :  (c) University of Glasgow 2006
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Esa Ilari vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32 Console API
--
-----------------------------------------------------------------------------

module System.Win32.Console (
	-- * Console code pages
	getConsoleCP,
	setConsoleCP,
	getConsoleOutputCP,
	setConsoleOutputCP,
  ) where

import System.Win32.Types

foreign import stdcall unsafe "windows.h GetConsoleCP"
	getConsoleCP :: IO UINT

foreign import stdcall unsafe "windows.h SetConsoleCP"
	setConsoleCP :: UINT -> IO ()

foreign import stdcall unsafe "windows.h GetConsoleOutputCP"
	getConsoleOutputCP :: IO UINT

foreign import stdcall unsafe "windows.h SetConsoleOutputCP"
	setConsoleOutputCP :: UINT -> IO ()

-- ToDo: lots more
