module HPC.Combine
  ( CombineOptions(..)
  , default_combine_options
  , hpc_combine
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Trace.Hpc.Tix

import Utils

data CombineOptions = CombineOptions
  { co_includeMods :: Set.Set String
  , co_excludeMods :: Set.Set String
  , co_combineFun :: CombineFun
  , co_mergeModule :: MergeFun
  , co_firstFile :: FilePath
  , co_secondFile :: FilePath
  , co_outputFile :: Maybe FilePath
  }

default_combine_options :: CombineOptions
default_combine_options = CombineOptions
  { co_includeMods = Set.empty
  , co_excludeMods = Set.empty
  , co_combineFun = ADD
  , co_mergeModule = INTERSECTION
  , co_firstFile = ""
  , co_secondFile = ""
  , co_outputFile = Nothing
  }

hpc_combine :: CombineOptions -> IO ()
hpc_combine _opts = return ()


------------------------------------------------------------------

data CombineFun = ADD | DIFF | SUB
     deriving (Eq,Show, Read, Enum)

theCombineFun :: CombineFun -> Integer -> Integer -> Integer
theCombineFun fn = case fn of
            ADD  -> \ l r -> l + r
            SUB  -> \ l r -> max 0 (l - r)
            DIFF -> \ g b -> if g > 0 then 0 else min 1 b

combine_main :: CombineOptions -> IO ()
combine_main opts  = do
  let f = theCombineFun (co_combineFun opts)

  Just tix1 <- readTix (co_firstFile opts)
  Just tix2 <- readTix (co_secondFile opts)

  let tix = mergeTix (co_mergeModule opts)
                     f
                     (filterTix (co_includeMods opts) (co_excludeMods opts) tix1)
                     (filterTix (co_includeMods opts) (co_excludeMods opts) tix2)

  case co_outputFile opts of
    Nothing -> putStrLn (show tix)
    Just out -> writeTix out tix

