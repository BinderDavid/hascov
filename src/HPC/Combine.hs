module HPC.Combine
  ( CombineOptions
  , hpc_combine
  , combineParser
  ) where

import Data.Set ( Set )
import qualified Data.Set as Set
import Options.Applicative
import Trace.Hpc.Tix ( readTix, writeTix )

import Utils ( MergeFun(..), filterTix, mergeTix )

--------------------------------------------------------------------------------
-- The different functions that hpc-combine supports
--------------------------------------------------------------------------------

data CombineFun = ADD | DIFF | SUB
     deriving (Eq,Show, Read, Enum)

applyCombineFun :: CombineFun -> Integer -> Integer -> Integer
applyCombineFun ADD  x y = x + y
applyCombineFun DIFF x y = max 0 (x - y)
applyCombineFun SUB  x y = if x > 0 then 0 else min 1 y

--------------------------------------------------------------------------------
-- The complete set of options for hpc-combine
--------------------------------------------------------------------------------

data CombineOptions = CombineOptions
  { co_includeMods :: Set String
  , co_excludeMods :: Set String
  , co_combineFun  :: CombineFun
  , co_mergeModule :: MergeFun
  , co_outputFile  :: Maybe FilePath
  , co_firstFile   :: FilePath
  , co_secondFile  :: FilePath
  }

--------------------------------------------------------------------------------
-- Worker function for the subcommand:
-- hascov hpc-combine
--------------------------------------------------------------------------------

hpc_combine :: CombineOptions -> IO ()
hpc_combine opts = do
  let f = applyCombineFun (co_combineFun opts)

  Just tix1 <- readTix (co_firstFile opts)
  Just tix2 <- readTix (co_secondFile opts)

  let tix = mergeTix (co_mergeModule opts)
                     f
                     (filterTix (co_includeMods opts) (co_excludeMods opts) tix1)
                     (filterTix (co_includeMods opts) (co_excludeMods opts) tix2)

  case co_outputFile opts of
    Nothing  -> putStrLn (show tix)
    Just out -> writeTix out tix

--------------------------------------------------------------------------------
-- Parser for the subcommand:
-- hascov hpc-combine
--------------------------------------------------------------------------------

parseSet :: Ord a => Parser a -> Parser (Set.Set a)
parseSet p =  Set.fromList <$> (many p)

parseIncludeMods :: Parser (Set.Set String)
parseIncludeMods = parseSet (strOption ( long "include" <> metavar "[PACKAGE:][MODULE]" <> help "include MODULE and/or PACKAGE"))

parseExcludeMods :: Parser (Set.Set String)
parseExcludeMods = parseSet (strOption ( long "exclude" <> metavar "[PACKAGE:][MODULE]" <> help "exclude MODULE and/or PACKAGE"))

parseOutputFile :: Parser (Maybe FilePath)
parseOutputFile = (maybe Nothing Just) <$> (optional $ strOption ( long "output" <> metavar "FILE" <> help "output FILE"))

parseCombineFun :: Parser CombineFun
parseCombineFun = option auto ( long "function"
                              <> metavar "FUNCTION"
                              <> help "combine .tix files with join function, FUNCTION = ADD | DIFF | SUB"
                              <> value ADD
                              <> showDefault )
parseMergeFun :: Parser MergeFun
parseMergeFun = option auto ( long "union"
                            <> metavar "MERGE"
                            <> help "Use the merge function of the module namespace, MERGE = UNION | INTERSECTION"
                            <> value INTERSECTION
                            <> showDefault )

parseArgument :: Parser FilePath
parseArgument = argument str (metavar "TIX_FILE")

combineParser :: Parser CombineOptions
combineParser = CombineOptions <$> parseIncludeMods <*> parseExcludeMods <*> parseCombineFun <*> parseMergeFun <*> parseOutputFile <*> parseArgument <*> parseArgument

