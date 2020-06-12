module Args where

import HPC.Combine (CombineOptions(..))

import Options.Applicative

data Commands
  =  HPCCombine CombineOptions

parser :: ParserInfo Commands
parser = info ((subparser subparsers) <**> helper) (fullDesc <> header "hascov -- haskell coverage")
  where
    subparsers = combineSubParser

--------------------------------------------------------------------------------
-- Parser for the subcommand:
-- hascov hpc-combine
--------------------------------------------------------------------------------

combineParser :: Parser Commands
combineParser = pure (HPCCombine CombineOptions)

combineSubParser :: Mod CommandFields Commands
combineSubParser = command "hpc-combine" (info (combineParser <**> helper) (progDesc "Combine two .tix files in a single .tix file"))
