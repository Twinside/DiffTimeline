type ErrorReturn = { error: string };

type Ref = string;

type SubModification = { beg: number, end: number };

type DiffCommand =
    { way: string,
      orig_idx: number,
      dest_idx: number,
      size: number,
      sub: SubModification[][] };

type DiffWithContext =
  { way: string,
    orig_idx: number,
    dest_idx: number,
    size: number,
    sub: SubModification[][],
    data: string[] };

type CommitTreeDiff =
    { kind: string,
      name: string,
      hash: Ref,
      binary: boolean,
      children: CommitTreeDiff[],
      diff: DiffWithContext[],
      // added by guy
      key: Ref,
      full_path: string
    };

type CommitDetailInfo =
    { parent_commit: Ref[],
      message: string,
      key: Ref,
      author: string,
      timestamp: number };

type CommitDetail = 
    { message: string,
      parents_sha: CommitDetailInfo[],
      key: Ref,
      author: string,
      file_changes: CommitTreeDiff[],
      timestamp: number,
      timezone: number };

/** @enum {string} */
var DiffAction = {
   DiffAddition: "+",
   DiffDeletion: "-",
   DiffNeutral: "="
};

type CommitPath =
    { commit: Ref,
      parent_commit: Ref[],
      message: string,
      author: string,
      timestamp: number,
      timezone: number,

      // additional field added by the GUI
      commit_date: string,
      splited_message: string,
     };

type ParentCommit =
    { parent_commit: Ref[],
      message: string,
      key: Ref,
      author: string,
      timestamp: number };

type ParentFile = 
    { data: string,
      binary: boolean,
      filekey: Ref,
      filename: string,
      parent_commit: ParentCommit[],
      message: string,
      diff: DiffCommand[],
      path: CommitPath[],
      key: Ref,
      author: string,
      timestamp: number,
      timezone: number,

      // added by the GUI (bad)
      full_path: string,
      name: string,
      file: string
    
  };

type CommitOverview =
    { parent_commit: Ref[],
      message: string,
      key: Ref,
      author: string,
      timestamp: number };

type BranchInfo = { name: string, key: Ref };

type RemoteBranches = { name: string, branches: BranchInfo[] };

type FileComparison = 
    { data_orig: string,
      ref_orig: Ref,
      data_dest: string,
      ref_dest: Ref,
      diff: DiffCommand[] };

type BlameRangeSource =
    { idx: number,
      size: number,
      orig_idx: number,
      padd_string: string
      tag: CommitOverview };

type BlameInfo = 
    { data: string,
      ranges: BlameRangeSource[],
      filename: string,
      earliest: number,
      latest: number };

