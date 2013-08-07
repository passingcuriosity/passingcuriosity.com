{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Hakyll.Git
       ( GitCommit (..)
       , gitCommitsCompiler
       , gitLogCompiler
       ) where

import           Control.Applicative (Applicative, (<$>), (<*>))
import           Data.Binary
import           Data.Generics (Data, Typeable)

import           Data.Time.Clock
import           Data.Time.Git (approxidate, posixToUTC)

import           Hakyll

-- | Important data from a git commit.
data GitCommit = GitCommit { commitId :: String -- ^ SHA identifier
                           , commitAuthor :: String -- ^ Author
                           , commitDate :: String -- ^ Time stamp
                           , commitMessage :: String -- ^ Commit message
                           }
         deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Binary GitCommit where
    put (GitCommit i a d m) = put i >> put a >> put (show d) >> put m
    get            = GitCommit <$> get <*> get <*> get <*> get

-- | Compile a log of git commits which modify a resource.
gitCommitsCompiler :: Compiler ([GitCommit])
gitCommitsCompiler = cached cacheName $ do
  path <- getResourceFilePath
  log <- unixFilter "git" ["log", path] ""
  return []
  where
    cacheName = "Hakyll.Web.Page.gitCommitsCompiler"

-- | Compile a simply-formatted log of git commits which modify a resource.
gitLogCompiler :: Compiler (Item String)
gitLogCompiler = gitCommitsCompiler >>= makeItem . show 
