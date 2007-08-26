-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- An FFI binding to the system part of the Win32 API.
--
-----------------------------------------------------------------------------

module System.Win32
	( module System.Win32.DLL
	, module System.Win32.File
	, module System.Win32.FileMapping
	, module System.Win32.Info
	, module System.Win32.Mem
	, module System.Win32.NLS
	, module System.Win32.Process
	, module System.Win32.Registry
	, module System.Win32.Time
	, module System.Win32.Console
	, module System.Win32.Security
	, module System.Win32.Types
	) where

import System.Win32.DLL
import System.Win32.File
import System.Win32.FileMapping
import System.Win32.Info
import System.Win32.Mem
import System.Win32.NLS hiding  ( LCID, LANGID, SortID, SubLANGID
                                , PrimaryLANGID, mAKELCID, lANGIDFROMLCID
                                , sORTIDFROMLCID, mAKELANGID, pRIMARYLANGID
                                , sUBLANGID )
import System.Win32.Process
import System.Win32.Registry
import System.Win32.Time
import System.Win32.Console
import System.Win32.Types
import System.Win32.Security

----------------------------------------------------------------
-- End
----------------------------------------------------------------
