{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Difftimeline.Externs( ErrorReturn( .. ) ) where

import Prelude

import Data.Aeson( ToJSON(..), object, (.=) )
import Data.Git
import Data.Git.Ref
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

instance ToJSON Ref where
    toJSON v = toJSON . decodeUtf8 $ toHex v

instance ToJSON CommitTreeDiff where
    toJSON ctd = object
        [ "kind"     .= treeDiffToKind ctd
        , "name"     .= treeDiffToName ctd
        , "hash"     .= treeDiffToRef ctd
        , "binary"   .= treeDiffIsBinary ctd
        , "children" .= treeDiffChildren ctd
        , "diff"     .= map DiffWithContext (treeDiffToDiffs ctd)
        ]

instance ToJSON CommitDetail where
    toJSON d = object
        [ "message"      .= commitDetailMessage d
        , "parents_sha"  .= commitDetailParents d
        , "key"          .= commitDetailKey d
        , "author"       .= commitDetailAuthor d
        , "file_changes" .= commitDetailChanges d
        , "timestamp"    .= commitDetailTimestamp d
        , "timezone"     .= commitDetailTimezone d
        ]

instance ToJSON CommitPath where
    toJSON cp  = object
             [ "commit"        .= pathCommitRef cp
             , "parent_commit" .= pathParentRef cp
             , "message"       .= pathMessage cp
             , "author"        .= pathAuthor cp
             , "timestamp"     .= pathTimestamp cp
             , "timezone"      .= pathTimezone cp
             ]

instance ToJSON CommitOverview where
    toJSON co = object
        [ "parent_commit" .= commitOverviewParent co
        , "message"       .= commitOverviewMessage co
        , "key"           .= commitOverviewRef co
        , "author"        .= commitOverviewAuthor co
        , "timestamp"     .= commitOverviewTimestamp co
        ]

instance ToJSON DiffAction where
    toJSON = toJSON . wayText where 
      wayText v = case v of
        DiffAddition -> "+" :: T.Text
        DiffDeletion -> "-"
        DiffNeutral  -> "="

instance ToJSON SubModification where
    toJSON m = object [ "beg" .= beg m, "end" .= end m]
        where beg (SubModification b _) = b
              end (SubModification _ e) = e

newtype DiffWithContext =
        DiffWithContext (DiffCommand, V.Vector T.Text)

instance ToJSON DiffWithContext where
    toJSON dc = object
             [ "way"      .= wayExtract dc
             , "orig_idx" .= orig dc
             , "dest_idx" .= dest dc
             , "size"     .= size dc
             , "sub"      .= sub dc
             , "data"     .= dataExtract dc
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
    toJSON c = object
             [ "way"      .= wayExtract c
             , "orig_idx" .= orig c
             , "dest_idx" .= dest c
             , "size"     .= size c
             , "sub"      .= sub c
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
    toJSON bi =
        object ["name" .= branchName bi
               ,"key"  .= branchRef bi
               ]

instance ToJSON RemoteBranches where
    toJSON rb =
        object ["name"     .= remoteName rb
               ,"branches" .= remoteBranches rb
               ]

instance (ToJSON tag) => ToJSON (BlameRangeSource tag) where
    toJSON b =
        object [ "idx"      .= sourceLineIndex b
               , "size"     .= sourceSize b
               , "orig_idx" .= sourceOriginalIndex b
               , "tag"      .= sourceTag b
               ]

instance ToJSON BlameInfo where
    toJSON b =
        object ["data"     .= blameData b
               ,"ranges"   .= blameRanges b
               ,"filename" .= blameFilename b
               ,"earliest" .= blameEarlyStamp b
               ,"latest"   .= blameLatestStamp b
               ]

instance ToJSON FileComparison where
    toJSON f =
      object [ "data_orig" .= comparisonFile1 f
             , "ref_orig"  .= comparisonRef1 f
             , "data_dest" .= comparisonFile2 f
             , "ref_dest"  .= comparisonRef2 f
             , "diff" .= comparisonDiff f
             ]

instance ToJSON ParentFile where
    toJSON f =
      object ["data"          .= fileData f
             ,"binary"        .= fileDataIsBinary f
             ,"filekey"       .= fileRef f
             ,"filename"      .= fileName f
             ,"parent_commit" .= parentRef f
             ,"message"       .= fileMessage f
             ,"diff"          .= fileDiff f
             ,"path"          .= commitPath f
             ,"key"           .= commitRef f
             ,"author"        .= parentCommitAuthor f
             ,"timestamp"     .= parentCommitTimestamp f
             ,"timezone"      .= parentCommmitTimezone f
             ]

