{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs

data Args = 
    Write { path :: FilePath }
  | Read
  deriving (Show, Data, Typeable)

writeMode = mode $ Write { path = def &= argPos 0 & typFile }
readMode = mode $ Read

main = do
  print =<< cmdArgs "sstable" [writeMode, readMode]