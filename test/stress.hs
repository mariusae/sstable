{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B8
import Data.SSTable.Writer
import Text.Printf
import Control.Exception
import System.CPUTime
import Data.List (sort)
import Data.String (fromString)
import System.IO
import Control.Monad

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

main = do
  print "generating.."

  -- let !sorted = sort [(fromString . show $ x, fromString $ "hey!!! " ++ (show x)) | x <- [1..1000000]]
  ---let !sorted = sort [(fromString . show $ x, fromString $ "hey!!! " ++ (show x)) | x <- [1..1000000]] :: [(String, String)]
  let !sorted = sort [show $ x | x <- [1..1000000]] :: [String]

  print "writing.."

  time $ do
    w <- openWriter "/tmp/test"
    forM_ sorted $ \k -> do
      writeEntry w (B8.pack k, B8.pack k)

    closeWriter w

  print "..."
  -- print "reading.."
  -- handle <- time $ Data.SSTable.read "/tmp/test"
  -- -- print handle
  -- print "finished reading"


  where
    isOrdered (x:[]) = True
    isOrdered (x:(rest@(y:ys)))
      | x <= y     = isOrdered rest
      | otherwise = error ("whoops " ++ (show x) ++ ", " ++ (show y))

