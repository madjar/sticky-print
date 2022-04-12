{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import StickyPrint (StickyPrinter (putLn, putSticky), newStickyPrinter)
import System.IO (stdout)

main :: IO ()
main = do
  printer <- newStickyPrinter stdout
  wait
  putLn printer "A first message"
  wait
  putSticky printer "Sticky message"
  wait
  putLn printer "A message"
  wait
  putSticky printer "Multi-line\nsticky"
  wait
  putSticky printer "Changed again"
  wait
  putLn printer "Another message"
  wait
  putSticky printer ""

wait :: IO ()
wait = threadDelay 1000000