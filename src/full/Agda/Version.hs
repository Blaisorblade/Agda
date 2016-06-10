{-# LANGUAGE TemplateHaskell #-}
module Agda.Version where

import Data.Maybe (maybe)
import Data.Version
import Data.List

import Distribution.VcsRevision.Git
import Language.Haskell.TH.Syntax
import qualified Paths_Agda as PA

-- | The version of Agda.

gitVersion :: Maybe String
gitVersion = $(do
  v <- qRunIO getRevision
  lift $ case v of
    Nothing           -> Nothing
    Just (hash,True)  -> Just $ hash ++ " (with local modifications)"
    Just (hash,False) -> Just $ hash)

version :: String
version = (intercalate "." $ map show $
            versionBranch PA.version) ++
          maybe "" (". Git commit: " ++) gitVersion
