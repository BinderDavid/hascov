module Main where

import Args (Commands(..), parser)
import HPC.Combine (hpc_combine)
import HPC.Draft (hpc_draft)
import HPC.Map (hpc_map)
import HPC.Markup (hpc_markup)
import HPC.Overlay (hpc_overlay)
import HPC.Report (hpc_report)
import HPC.Show (hpc_show)
import HPC.Sum (hpc_sum)

import Options.Applicative (execParser)

main :: IO ()
main = do
 res <- execParser parser
 case res of
   HPCCombine options -> hpc_combine options
   HPCDraft   options -> hpc_draft options
   HPCMap     options -> hpc_map options
   HPCMarkup  options -> hpc_markup options
   HPCOverlay options -> hpc_overlay options
   HPCReport  options -> hpc_report options
   HPCShow    options -> hpc_show options
   HPCSum     options -> hpc_sum options
