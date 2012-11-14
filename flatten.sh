#!/bin/sh

# flatten the gh-pages history to avoid carrying enormous commits
git branch -D gh-pages
git co -b gh-pages origin/gh-pages
git reset --soft f8ca9fd
git commit -m "Flattening"
git push -f

