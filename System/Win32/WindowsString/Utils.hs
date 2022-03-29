{- |
   Module      :  System.Win32.Utils
   Copyright   :  2009 Balazs Komuves, 2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Utilities for calling Win32 API
-}
module System.Win32.WindowsString.Utils
  ( module System.Win32.WindowsString.Utils
  , module System.Win32.Utils
  ) where

import Foreign.C.Types             ( CInt )
import Foreign.Marshal.Array       ( allocaArray )
import Foreign.Ptr                 ( nullPtr )

import System.Win32.Utils hiding
  ( try
  , tryWithoutNull
  , trySized
  )
import System.Win32.WindowsString.String         ( LPTSTR, peekTString, peekTStringLen
                                   , withTStringBufferLen )
import System.Win32.WindowsString.Types          ( UINT
                                   , failIfZero
                                  )
import qualified System.Win32.WindowsString.Types ( try )
import System.OsString.Windows


-- | Support for API calls that are passed a fixed-size buffer and tell
-- you via the return value if the buffer was too small.  In that
-- case, we extend the buffer size and try again.
try :: String -> (LPTSTR -> UINT -> IO UINT) -> UINT -> IO WindowsString
try = System.Win32.WindowsString.Types.try
{-# INLINE try #-}

tryWithoutNull :: String -> (LPTSTR -> UINT -> IO UINT) -> UINT -> IO WindowsString
tryWithoutNull loc f n = do
   e <- allocaArray (fromIntegral n) $ \lptstr -> do
          r <- failIfZero loc $ f lptstr n
          if r > n then return (Left r) else do
            str <- peekTString lptstr
            return (Right str)
   case e of
        Left r'   -> tryWithoutNull loc f r'
        Right str -> return str

-- | Support for API calls that return the required size, in characters
-- including a null character, of the buffer when passed a buffer size of zero.
trySized :: String -> (LPTSTR -> CInt -> IO CInt) -> IO WindowsString
trySized wh f = do
    c_len <- failIfZero wh $ f nullPtr 0
    let len = fromIntegral c_len
    withTStringBufferLen len $ \(buf', len') -> do
        let c_len' = fromIntegral len'
        c_len'' <- failIfZero wh $ f buf' c_len'
        let len'' = fromIntegral c_len''
        peekTStringLen (buf', len'' - 1) -- Drop final null character
