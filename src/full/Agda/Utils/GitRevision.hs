module Agda.Utils.GitRevision (gitDescribe) where

-- Adapted from
-- https://github.com/jkff/vcs-revision/blob/master/Distribution/VcsRevision/Git.hs

import Control.Exception ( IOException, try )
import System.Process ( readProcessWithExitCode )
import System.Exit ( ExitCode(..) )

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

-- | Nothing if we're not in a git repo, Just (output of git-describe) if we're in a repo.
gitDescribe :: IO (Maybe String)
gitDescribe = do
  res <- tryIO $ readProcessWithExitCode "git" ["describe", "--dirty", "--always", "--match", "*.*.*"] ""
  case res of
    Left ex -> return Nothing
    Right (ExitSuccess, description, _) -> do
      return $ Just $ init description
    _ -> return Nothing
