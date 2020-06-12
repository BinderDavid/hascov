module Utils where

import Trace.Hpc.Tix
import qualified Data.Set as Set
import qualified Data.Map as Map

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

allowModule :: Set.Set String -- ^ The included modules
            -> Set.Set String -- ^ The excluded modules
            -> String
            -> Bool
allowModule im em full_mod
      | full_mod' `Set.member` em = False
      | pkg_name  `Set.member` em = False
      | mod_name  `Set.member` em = False
      | Set.null               im = True
      | full_mod' `Set.member` im = True
      | pkg_name  `Set.member` im = True
      | mod_name  `Set.member` im = True
      | otherwise                 = False
  where
          full_mod' = pkg_name ++ mod_name
      -- pkg name always ends with '/', main
          (pkg_name,mod_name) =
                        case span (/= '/') full_mod of
                     (p,'/':m) -> (p ++ ":",m)
                     (m,[])    -> (":",m)
                     _         -> error "impossible case in allowModule"

filterTix :: Set.Set String -- ^ The included modules
          -> Set.Set String -- ^ The excluded modules
          -> Tix
          -> Tix
filterTix im em (Tix tixs) =
     Tix $ filter (allowModule im em . tixModuleName) tixs

