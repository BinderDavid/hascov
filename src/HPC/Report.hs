module HPC.Report where

data ReportOptions = ReportOptions

hpc_report :: ReportOptions -> IO ()
hpc_report _ = putStrLn ""
