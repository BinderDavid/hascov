module HPC.Markup where

data MarkupOptions = MarkupOptions

hpc_markup :: MarkupOptions -> IO ()
hpc_markup _ = putStrLn ""
