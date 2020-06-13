module HPC.Show where

data ShowOptions = ShowOptions

hpc_show :: ShowOptions -> IO ()
hpc_show _ = putStrLn ""
