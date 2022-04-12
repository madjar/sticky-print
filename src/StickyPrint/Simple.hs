-- | A simplified interface to StickyPrint, where you don't have to create a printer youself.
-- Prints to stdout
module StickyPrint.Simple (putLn, putSticky) where

import Data.ByteString (ByteString)
import qualified StickyPrint
import System.IO (stdout)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE printer #-}
printer :: StickyPrint.StickyPrinter
printer = unsafePerformIO (StickyPrint.newStickyPrinter stdout)

-- | Print a line to stdout. This will add a newline at the end.
-- It behaves like 'putStrLn', except it handles the logic for the sticky.
putLn :: ByteString -> IO ()
putLn = StickyPrint.putLn printer

-- | Print the given string as "sticky", meaning it will always stay at the bottom of the terminal.
putSticky :: ByteString -> IO ()
putSticky = StickyPrint.putSticky printer