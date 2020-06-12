module HPC.Map
  ( MapOptions
  , hpc_map
  , mapParser
  ) where

import Trace.Hpc.Tix (Tix(..), TixModule(..), readTix, writeTix)
import Data.Set (Set)
import Utils
import Options.Applicative

--------------------------------------------------------------------------------
-- The different functions that hpc-map supports
--------------------------------------------------------------------------------

data MapFun = ID | INV | ZERO
  deriving (Eq,Show, Read, Enum)

applyMapFun :: MapFun -> Integer -> Integer
applyMapFun ID   x = x
applyMapFun INV  0 = 1
applyMapFun INV  _ = 0
applyMapFun ZERO _ = 0

--------------------------------------------------------------------------------
-- The complete set of options for hpc-map
--------------------------------------------------------------------------------

data MapOptions = MapOptions
  { mf_includeMods :: Set String
  , mf_excludeMods :: Set String
  , mf_outputFile  :: Maybe FilePath
  , mf_mapFun      :: MapFun
  , mf_arg         :: FilePath
  }

hpc_map :: MapOptions -> IO ()
hpc_map opts = do
    let f = applyMapFun (mf_mapFun opts)

    Just tix <- readTix (mf_arg opts)

    let (Tix inside_tix) = filterTix (mf_includeMods opts) (mf_excludeMods opts) tix
    let tix' = Tix [ TixModule m p i (map f t)
                   | TixModule m p i t <- inside_tix
                   ]
    case mf_outputFile opts of
      Nothing -> putStrLn (show tix')
      Just fp -> writeTix fp tix'

--------------------------------------------------------------------------------
-- Parser for the subcommand:
-- hascov hpc-map
--------------------------------------------------------------------------------

parseMapFun :: Parser MapFun
parseMapFun = option auto ( long "function"
                              <> metavar "FUNCTION"
                              <> help "apply function to .tix files, FUNCTION = ID | INV | ZERO"
                              <> value ID
                              <> showDefault )
mapParser :: Parser MapOptions
mapParser = MapOptions <$> parseIncludeMods <*> parseExcludeMods <*> parseOutputFile <*> parseMapFun <*> parseArgument
