module Args where

import HPC.Combine (CombineOptions, combineParser)
import HPC.Draft (DraftOptions)
import HPC.Map (MapOptions, mapParser)
import HPC.Markup (MarkupOptions)
import HPC.Overlay (OverlayOptions)
import HPC.Report (ReportOptions)
import HPC.Show (ShowOptions)
import HPC.Sum (SumOptions)

import Options.Applicative

data Commands
  = HPCCombine CombineOptions
  | HPCDraft DraftOptions
  | HPCMap MapOptions
  | HPCMarkup MarkupOptions
  | HPCOverlay OverlayOptions
  | HPCReport ReportOptions
  | HPCShow ShowOptions
  | HPCSum SumOptions

parser :: ParserInfo Commands
parser = info ((subparser subparsers) <**> helper) (fullDesc <> header "hascov -- haskell coverage")
  where
    subparsers = combineSubParser <> mapSubParser

--------------------------------------------------------------------------------
-- Parser for the subcommand:
-- hascov hpc-combine
--------------------------------------------------------------------------------

combineSubParser :: Mod CommandFields Commands
combineSubParser = command "hpc-combine" (info ((HPCCombine <$> combineParser) <**> helper) (progDesc "Combine two .tix files in a single .tix file"))

--------------------------------------------------------------------------------
-- Parser for the subcommand:
-- hascov hpc-map
--------------------------------------------------------------------------------

mapSubParser :: Mod CommandFields Commands
mapSubParser = command "hpc-map" (info ((HPCMap <$> mapParser) <**> helper) (progDesc "Map a function over a single .tix file"))
