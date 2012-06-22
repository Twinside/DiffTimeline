{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Difftimeline.Externs( difftimelineEnv ) where

import Prelude

import Data.Git
import qualified Data.Text as T
import Difftimeline.GitQuery
import Difftimeline.Diff
import Text.Language.Closure

data ErrorReturn = ErrorReturn
    { errorMessage :: String
    }

treeDiffToKind :: CommitTreeDiff -> T.Text
treeDiffToKind (TreeElement _ _ _) = "neutral"
treeDiffToKind (NeutralElement _ _) = "neutral"
treeDiffToKind (AddElement _ _ ) = "addition"
treeDiffToKind (DelElement _ _) = "deletion"
treeDiffToKind (ModifyBinaryElement _ _) = "modification"
treeDiffToKind (ModifyElement _ _ _) = "modification"

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

instance ClosureDescriptable ErrorReturn Serializable where
    typename _ = "ErrorReturn"
    toClosureDesc _ = record [ "error" .: errorMessage ]

instance ClosureDescriptable Ref Serializable where
    typename _ = "Ref"
    toClosureDesc _ = value (decodeUtf8 . toHex)

instance ClosureDescriptable CommitTreeDiff Serializable where
    typename _ = "CommitTreeDiff"
    toClosureDesc _ = record
        [ "kind"   .: treeDiffToKind
        , "name"   .: treeDiffToName
        , "hash"   .: treeDiffToRef
        , "binary" .: treeDiffIsBinary
        ]

instance ClosureDescriptable CommitDetail Serializable where
    typename _ = "CommitDetail"
    toClosureDesc _ = record
        [ "message"      .: commitDetailMessage
        , "parents_sha"  .: commitDetailParents
        , "key"          .: commitDetailKey
        , "author"       .: commitDetailAuthor
        , "file_changes" .: commitDetailChanges
        , "timestamp"    .: commitDetailTimestamp
        , "timezone"     .: commitDetailTimezone
        ]

instance ClosureDescriptable CommitPath Serializable where
    typename _ = "CommitPath"
    toClosureDesc _  = record
             [ "commit"        .: pathCommitRef
             , "parent_commit" .: pathParentRef
             , "message"       .: pathMessage
             , "author"        .: pathAuthor
             , "timestamp"     .: pathTimestamp
             , "timezone"      .: pathTimezone
             ]

instance ClosureDescriptable CommitOverview Serializable where
    typename _ = "CommitOverview"
    toClosureDesc _ = record
        [ "parent_commit" .: commitOverviewParent
        , "message"       .: commitOverviewMessage
        , "key"           .: commitOverviewRef
        , "author"        .: commitOverviewAuthor
        , "timestamp"     .: commitOverviewTimestamp
        ]

instance ClosureDescriptable DiffAction Serializable where
    typename _ = "DiffAction"
    toClosureDesc _ =
        enum [DiffAddition, DiffDeletion, DiffNeutral] show wayText
            where wayText DiffAddition = "+" :: T.Text
                  wayText DiffDeletion = "-"
                  wayText DiffNeutral  = "="

instance ClosureDescriptable SubModification Serializable where
    typename _ = "SubModification"
    toClosureDesc _ = record [ "beg" .: beg, "end" .: end ]
        where beg (SubModification b _) = b
              end (SubModification _ e) = e

instance ClosureDescriptable DiffCommand Serializable where
    typename _ = "DiffCommand"
    toClosureDesc _ = record
             [ "way"      .: wayExtract
             , "orig_idx" .: orig
             , "dest_idx" .: dest
             , "size"     .: size
             , "sub"      .: sub
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

instance ClosureDescriptable ParentFile Serializable where
    typename _ = "ParentFile"
    toClosureDesc _ =
      record ["data"          .: fileData
             ,"filekey"       .: fileRef
             ,"parent_commit" .: (last . parentRef)
             ,"message"       .: fileMessage
             ,"diff"          .: fileDiff
             ,"path"          .: commitPath
             ,"key"           .: commitRef
             ,"author"        .: parentCommitAuthor
             ,"timestamp"     .: parentCommitTimestamp
             ,"timezone"      .: parentCommmitTimezone
             ]

difftimelineEnv :: ClosTypingEnvironment ()
difftimelineEnv = do
    declare (undefined :: ErrorReturn)
    declare (undefined :: Ref)
    declare (undefined :: CommitTreeDiff)
    declare (undefined :: CommitDetail)
    declare (undefined :: DiffAction)
    declare (undefined :: SubModification)
    declare (undefined :: DiffCommand)
    declare (undefined :: CommitPath)
    declare (undefined :: ParentFile)
    declare (undefined :: CommitOverview)

