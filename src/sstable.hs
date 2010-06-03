{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import qualified Data.ByteString as B
import System.IO (isEOF)
import Data.Char (isSpace, chr)
import Control.Monad (forM_, unless)

import Data.SSTable.Writer
import Data.SSTable.Reader

data Args 
  = Write { path :: FilePath }
  | Read  { path :: FilePath }
  deriving (Show, Data, Typeable)

writeMode = mode $ Write { path = def &= argPos 0 & typFile }
readMode  = mode $ Read  { path = def &= argPos 0 & typFile }

main = doit =<< cmdArgs "sstable" [writeMode, readMode]

doit (Write path) = withWriter path go
  where
    go writer = do
      eof <- isEOF
      unless eof $ parseLine writer >> go writer

    -- TODO: strip whitespace.

    parseLine writer = 
      writeEntry writer . B.break (isSpace . chr . fromIntegral) =<< B.getLine

doit (Read path) = withReader path enum
  where 
    enum reader = do
      entries <- query reader B.empty
      forM_ entries $ \(key, value) -> do
        B.putStr key
        putStr " => "
        B.putStrLn value
