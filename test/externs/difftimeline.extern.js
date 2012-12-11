/** @typedef {{ error: string }} */
var ErrorReturn;

/** @typedef {string} */
var Ref;

/** @typedef {{ beg: number,
                end: number }} */
var SubModification;

/** @typedef {{ way: string,
                orig_idx: number,
                dest_idx: number,
                size: number,
                sub: Array.<Array.<SubModification>> }} */
var DiffCommand;

/** @typedef {{ way: string,
                orig_idx: number,
                dest_idx: number,
                size: number,
                sub: Array.<Array.<SubModification>>,
                data: Array.<string> }} */
var DiffWithContext;

/** @typedef {{ kind: string,
                name: string,
                hash: Ref,
                binary: boolean,
                children: Array.<CommitTreeDiff>,
                diff: Array.<DiffWithContext> }} */
var CommitTreeDiff;

/** @typedef {{ message: string,
                parents_sha: Array.<{ parent_commit: Array.<Ref>,
                message: string,
                key: Ref,
                author: string,
                timestamp: number }>,
                key: Ref,
                author: string,
                file_changes: Array.<CommitTreeDiff>,
                timestamp: number,
                timezone: number }} */
var CommitDetail;

/** @enum {string} */
var DiffAction = {
   DiffAddition: "+",
   DiffDeletion: "-",
   DiffNeutral: "="
};

/** @typedef {{ commit: Ref,
                parent_commit: Array.<Ref>,
                message: string,
                author: string,
                timestamp: number,
                timezone: number }} */
var CommitPath;

/** @typedef {{ data: string,
                binary: boolean,
                filekey: Ref,
                filename: string,
                parent_commit: Array.<{ parent_commit: Array.<Ref>,
                message: string,
                key: Ref,
                author: string,
                timestamp: number }>,
                message: string,
                diff: Array.<DiffCommand>,
                path: Array.<CommitPath>,
                key: Ref,
                author: string,
                timestamp: number,
                timezone: number }} */
var ParentFile;

/** @typedef {{ parent_commit: Array.<Ref>,
                message: string,
                key: Ref,
                author: string,
                timestamp: number }} */
var CommitOverview;

/** @typedef {{ name: string,
                key: Ref }} */
var BranchInfo;

/** @typedef {{ name: string,
                branches: Array.<BranchInfo> }} */
var RemoteBranches;

/** @typedef {{ data_orig: string,
                ref_orig: Ref,
                data_dest: string,
                ref_dest: Ref,
                diff: Array.<DiffCommand> }} */
var FileComparison;

/** @typedef {{ idx: number,
                size: number,
                orig_idx: number,
                tag: CommitOverview }} */
var BlameRangeSource;

/** @typedef {{ data: string,
                ranges: Array.<BlameRangeSource>,
                filename: string,
                earliest: number,
                latest: number }} */
var BlameInfo;

