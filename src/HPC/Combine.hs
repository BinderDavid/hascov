module HPC.Combine
  ( CombineOptions(..)
  , default_combine_options
  , hpc_combine
  ) where


import Trace.Hpc.Tix

import qualified Data.Set as Set
import qualified Data.Map as Map

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
                     (filterTix opts tix1)
                     (filterTix opts tix2)

  case co_outputFile opts of
    Nothing -> putStrLn (show tix)
    Just out -> writeTix out tix

data MergeFun = INTERSECTION | UNION
     deriving (Eq,Show, Read, Enum)

theMergeFun :: (Ord a) => MergeFun -> Set.Set a -> Set.Set a -> Set.Set a
theMergeFun INTERSECTION = Set.intersection
theMergeFun UNION        = Set.union

mergeTix :: MergeFun
         -> (Integer -> Integer -> Integer) -> Tix -> Tix -> Tix
mergeTix modComb f
         (Tix t1)
         (Tix t2)  = Tix
         [ case (Map.lookup m fm1,Map.lookup m fm2) of
           -- todo, revisit the semantics of this combination
            (Just (TixModule _ hash1 len1 tix1),Just (TixModule _ hash2 len2 tix2))
               | hash1 /= hash2
               || length tix1 /= length tix2
               || len1 /= length tix1
               || len2 /= length tix2
                     -> error $ "mismatched in module " ++ m
               | otherwise      ->
                     TixModule m hash1 len1 (zipWith f tix1 tix2)
            (Just m1,Nothing) ->
                  m1
            (Nothing,Just m2) ->
                  m2
            _ -> error "impossible"
         | m <- Set.toList (theMergeFun modComb m1s m2s)
         ]
  where
   m1s = Set.fromList $ map tixModuleName t1
   m2s = Set.fromList $ map tixModuleName t2

   fm1 = Map.fromList [ (tixModuleName tix,tix)
                      | tix <- t1
                      ]
   fm2 = Map.fromList [ (tixModuleName tix,tix)
                      | tix <- t2
                      ]

-- filterModules takes a list of candidate modules,
-- and
--  * excludes the excluded modules
--  * includes the rest if there are no explicitly included modules
--  * otherwise, accepts just the included modules.

allowModule :: CombineOptions -> String -> Bool
allowModule opts full_mod
      | full_mod' `Set.member` co_excludeMods opts = False
      | pkg_name  `Set.member` co_excludeMods opts = False
      | mod_name  `Set.member` co_excludeMods opts = False
      | Set.null (co_includeMods opts)             = True
      | full_mod' `Set.member` co_includeMods opts = True
      | pkg_name  `Set.member` co_includeMods opts = True
      | mod_name  `Set.member` co_includeMods opts = True
      | otherwise                                  = False
  where
          full_mod' = pkg_name ++ mod_name
      -- pkg name always ends with '/', main
          (pkg_name,mod_name) =
                        case span (/= '/') full_mod of
                     (p,'/':m) -> (p ++ ":",m)
                     (m,[])    -> (":",m)
                     _         -> error "impossible case in allowModule"

filterTix :: CombineOptions -> Tix -> Tix
filterTix flags (Tix tixs) =
     Tix $ filter (allowModule flags . tixModuleName) tixs
