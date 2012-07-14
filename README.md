
Difftimeline
============

Let's start easy :

    `$ difftimeline`

![First page](tutorial/first.png)

You can see the diff of the working directory from the HEAD. If you
click the huge button `<`, you can fetch previous commit from the
current head

![Fetching previous commits](tutorial/fetch_previous_commit.png)

If you click the button above `<` you can switch back and forth to the
"compact" view.

![Compact commit view](tutorial/compact_commit.png)

Navigating files
----------------
Clicking on the `Tree` panel will show you all the files contained
in the commit and let you browse them.

![Commit tree](tutorial/commit_tree.png)

You can click on any to follow the history of a specific file, let's follow the `static-content/difftimeline.css`

![File diff](tutorial/file_diff.png)

You can see every modification of a file between two commits. In the commit description, each round represent a commit whom have not modified the file. If you click on any round, you will jump to the commit description. You can also use the compact mode

![File diff](tutorial/file_diff_compact.png)

To directly see modifications for a file, you can use `$ difftimeline filename` at the command line.


Comparing branches
------------------
This part is still 'experimental', so expect changes in the future, and there is still rough edges. To let you compare branch, launch difftimeline the following way : `difftimeline compare`

![Branch comparaison](tutorial/branch_compare.png)

You can then drag'n'drop branches in the square to compare them

![Branch comparaison](tutorial/branch_compare_result.png)

Modification
------------
To modify/hack difftimeline, see the [Build.md](Build.md) file.