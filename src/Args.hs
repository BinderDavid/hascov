module Args where

import HPC.Combine (CombineOptions, combineParser)
import HPC.Map (MapOptions, mapParser)

import Options.Applicative

data Commands
  = HPCCombine CombineOptions
  | HPCMap MapOptions

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
