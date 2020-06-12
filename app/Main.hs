module Main where

import Args (Commands(..), parser)
import HPC.Combine (hpc_combine)
import Options.Applicative (execParser)

main :: IO ()
main = do
 res <- execParser parser
 case res of
   HPCCombine options  -> hpc_combine options

