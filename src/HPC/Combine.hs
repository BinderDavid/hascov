module HPC.Combine
  ( CombineOptions(..)
  , hpc_combine
  ) where

data CombineOptions = CombineOptions

hpc_combine :: CombineOptions -> IO ()
hpc_combine _opts = return ()

