{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright: (c) 2022 Georges Dubus
-- SPDX-License-Identifier: MIT
-- Maintainer: Georges Dubus <georges.dubus@hey.com>
--
-- An output that sticks to the bottom of the terminal while the rest of your logs scroll by
module StickyPrint
  ( StickyPrinter,
    putLn,
    putSticky,
    newStickyPrinter,
  )
where

import Control.Concurrent (MVar, modifyMVar_, newMVar)
import Control.Monad (when)
import Data.ByteString (ByteString, count, hPut)
import System.Console.ANSI (hClearFromCursorToScreenEnd, hCursorUpLine)
import System.IO (Handle)

data StickyPrinter = StickyPrinter
  { putLn :: ByteString -> IO (),
    putSticky :: ByteString -> IO ()
  }

data PrinterState = PrinterState
  { stickyContent :: ByteString,
    stickyLinesCount :: Int
  }

newStickyPrinter :: Handle -> IO StickyPrinter
newStickyPrinter handle = do
  state <- newMVar (PrinterState "" 0)
  return
    StickyPrinter
      { putLn = putLnImpl state handle,
        putSticky = putStickyImpl state handle
      }

putLnImpl :: MVar PrinterState -> Handle -> ByteString -> IO ()
putLnImpl ref handle msg = modifyMVar_ ref $ \state -> do
  clear state handle
  hPut handle msg
  hPut handle "\n"
  putStickyMsg handle state
  return state

putStickyImpl :: MVar PrinterState -> Handle -> ByteString -> IO ()
putStickyImpl ref handle msg = modifyMVar_ ref $ \state -> do
  clear state handle
  let newState = state {stickyContent = msg, stickyLinesCount = count 0x0a msg + 1}
  putStickyMsg handle newState
  return newState

-- NOTE: we can't use hSaveCursor/hRestoreCursor because the position it uses gets invalidated
--       as soon as we have a newline

clear :: PrinterState -> Handle -> IO ()
clear PrinterState {stickyLinesCount} handle = do
  when (stickyLinesCount > 0) $
    hCursorUpLine handle stickyLinesCount
  hClearFromCursorToScreenEnd handle

putStickyMsg :: Handle -> PrinterState -> IO ()
putStickyMsg handle PrinterState {stickyContent} =
  when (stickyContent /= "") $ do
    hPut handle stickyContent
    hPut handle "\n"
