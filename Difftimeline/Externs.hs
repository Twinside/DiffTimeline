{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Difftimeline.Externs( ErrorReturn( .. ) ) where

import Prelude

import Data.Git
import Data.Aeson( ToJSON(..), object, (.=) )
import qualified Data.Text as T
import Difftimeline.GitQuery
import Difftimeline.Diff
import qualified Data.Vector as V

data ErrorReturn = ErrorReturn
    { errorMessage :: String
    }

treeDiffToKind :: CommitTreeDiff -> T.Text
treeDiffToKind (TreeElement {}) = "neutral"
treeDiffToKind (NeutralElement _ _) = "neutral"
treeDiffToKind (AddElement _ _ ) = "addition"
treeDiffToKind (DelElement _ _) = "deletion"
treeDiffToKind (ModifyBinaryElement _ _) = "modification"
treeDiffToKind (ModifyElement {}) = "modification"

treeDiffToName :: CommitTreeDiff -> T.Text
treeDiffToName (TreeElement n _ _) = n
treeDiffToName (NeutralElement n _) = n
treeDiffToName (AddElement n _ ) = n
treeDiffToName (DelElement n _) = n
treeDiffToName (ModifyBinaryElement n _) = n
treeDiffToName (ModifyElement n _ _) = n

treeDiffToRef :: CommitTreeDiff -> Ref
treeDiffToRef (TreeElement _ r _) = r
treeDiffToRef (NeutralElement _ r) = r
treeDiffToRef (AddElement _ r) = r
treeDiffToRef (DelElement _ r) = r
treeDiffToRef (ModifyBinaryElement _ r) = r
treeDiffToRef (ModifyElement _ r _) = r

treeDiffIsBinary :: CommitTreeDiff -> Bool
treeDiffIsBinary (ModifyBinaryElement _ _) = True
treeDiffIsBinary _ = False

treeDiffToDiffs :: CommitTreeDiff -> [(DiffCommand, V.Vector T.Text)]
treeDiffToDiffs (ModifyElement _ _ lst) = lst
treeDiffToDiffs _ = []

treeDiffChildren :: CommitTreeDiff -> [CommitTreeDiff]
treeDiffChildren (TreeElement _ _ lst) = lst
treeDiffChildren _ = []

instance ToJSON ErrorReturn where
    toJSON v = object [ "error" .= errorMessage v ]

instance ToJSON Ref where
    toJSON = toJSON . decodeUtf8 . toHex

instance ToJSON CommitTreeDiff where
    toJSON v = object
        [ "kind"     .= treeDiffToKind v
        , "name"     .= treeDiffToName v
        , "hash"     .= treeDiffToRef v
        , "binary"   .= treeDiffIsBinary v
        , "children" .= treeDiffChildren v
        , "diff"     .= map DiffWithContext (treeDiffToDiffs v)
        ]

instance ToJSON CommitPath where
    toJSON v  = object
             [ "commit"        .= pathCommitRef v
             , "parent_commit" .= pathParentRef v
             , "message"       .= pathMessage v
             , "author"        .= pathAuthor v
             , "timestamp"     .= pathTimestamp v
             , "timezone"      .= pathTimezone v
             ]

instance ToJSON CommitOverview where
    toJSON v = object
        [ "parents"       .= commitOverviewParent v
        , "message"       .= commitOverviewMessage v
        , "id"            .= commitOverviewRef v
        , "author"        .= commitOverviewAuthor v
        , "timestamp"     .= commitOverviewTimestamp v
        ]

instance ToJSON DiffAction where
    toJSON = toJSON . wayText
        where wayText DiffAddition = "+" :: T.Text
              wayText DiffDeletion = "-"
              wayText DiffNeutral  = "="

instance ToJSON SubModification where
    toJSON (SubModification b e) =
        object [ "beg" .= b, "end" .= e ]

newtype DiffWithContext =
        DiffWithContext (DiffCommand, V.Vector T.Text)

instance ToJSON DiffWithContext where
    toJSON v = object
             [ "way"      .= wayExtract v
             , "orig_idx" .= orig v
             , "dest_idx" .= dest v
             , "size"     .= size v
             , "sub"      .= sub v
             , "data"     .= dataExtract v
             ]
        where dataExtract (DiffWithContext (_, d)) = d

              wayExtract (DiffWithContext (DiffCommand way _ _ _, _)) = way
              wayExtract (DiffWithContext (DiffRefined way _ _ _ _, _)) = way

              orig (DiffWithContext (DiffCommand _ o _ _, _)) = o
              orig (DiffWithContext (DiffRefined _ o _ _ _, _)) = o

              dest (DiffWithContext (DiffCommand _ _ d _, _)) = d
              dest (DiffWithContext (DiffRefined _ _ d _ _, _)) = d

              size (DiffWithContext (DiffCommand _ _ _ s, _)) = s
              size (DiffWithContext (DiffRefined _ _ _ s _, _)) = s

              sub (DiffWithContext (DiffRefined _ _ _ _ l, _)) = l
              sub _ = []

instance ToJSON DiffCommand where
    toJSON v = object
             [ "way"      .= wayExtract v
             , "orig_idx" .= orig v
             , "dest_idx" .= dest v
             , "size"     .= size v
             , "sub"      .= sub v
             ]
        where wayExtract (DiffCommand way _ _ _) = way
              wayExtract (DiffRefined way _ _ _ _) = way

              orig (DiffCommand _ o _ _) = o
              orig (DiffRefined _ o _ _ _) = o

              dest (DiffCommand _ _ d _) = d
              dest (DiffRefined _ _ d _ _) = d

              size (DiffCommand _ _ _ s) = s
              size (DiffRefined _ _ _ s _) = s

              sub (DiffRefined _ _ _ _ l) = l
              sub _ = []

instance ToJSON BranchInfo where
    toJSON v =
        object ["name" .= branchName v
               ,"key"  .= branchRef v
               ]

instance ToJSON RemoteBranches where
    toJSON v =
        object [ "id" .= remoteName v
               , "branches" .= pack
               ]
       where pack = map embedded . zip [0..] $ remoteBranches v
             embedded (i, b) =
                object ["name" .= branchName b
                       ,"key"  .= branchRef b
                       ,"id"   .= (i :: Int)
                       ]
instance (ToJSON tag) => ToJSON (BlameRangeSource tag) where
    toJSON v =
        object [ "idx"      .= sourceLineIndex v
               , "size"     .= sourceSize v
               , "orig_idx" .= sourceOriginalIndex v
               , "tag"      .= sourceTag v
               ]

instance ToJSON BlameInfo where
    toJSON v =
        object ["data"     .= blameData v
               ,"ranges"   .= blameRanges v
               ,"filename" .= blameFilename v
               ,"earliest" .= blameEarlyStamp v
               ,"latest"   .= blameLatestStamp v
               ]

instance ToJSON FileComparison where
    toJSON v =
      object [ "data_orig" .= comparisonFile1 v
             , "ref_orig"  .= comparisonRef1 v
             , "data_dest" .= comparisonFile2 v
             , "ref_dest"  .= comparisonRef2 v
             , "diff" .= comparisonDiff v
             ]

instance ToJSON ParentFile where
    toJSON v =
      object ["data"          .= fileData v
             ,"binary"        .= fileDataIsBinary v
             ,"filekey"       .= fileRef v
             ,"filename"      .= fileName v
             ,"parent_commit" .= parentRef v
             ,"message"       .= fileMessage v
             ,"diff"          .= fileDiff v
             ,"path"          .= commitPath v
             ,"key"           .= commitRef v
             ,"author"        .= parentCommitAuthor v
             ,"timestamp"     .= parentCommitTimestamp v
             ,"timezone"      .= parentCommmitTimezone v
             ]

