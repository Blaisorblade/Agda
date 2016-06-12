{-# LANGUAGE TemplateHaskell #-}
module Agda.Version where

import Data.Maybe (maybe)
import Data.Version
import Data.List

import Agda.Utils.GitRevision
import Language.Haskell.TH.Syntax
import qualified Paths_Agda as PA

-- | The version of Agda.

gitVersion :: Maybe String
gitVersion = $(do
  v <- qRunIO gitDescribe
  lift $ v)

version :: String
version = (intercalate "." $ map show $
            versionBranch PA.version)

fullVersion :: String
fullVersion =
  case gitVersion of
    Nothing -> version
    Just ver ->
      if version `isPrefixOf` ver then
        ver
      else
        version ++ ". Git version: " ++ ver
