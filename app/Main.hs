module Main where

import Args (Commands(..), parser)
import HPC.Combine (hpc_combine)
import HPC.Map (hpc_map)
import Options.Applicative (execParser)

main :: IO ()
main = do
 res <- execParser parser
 case res of
   HPCCombine options -> hpc_combine options
   HPCMap     options -> hpc_map options

