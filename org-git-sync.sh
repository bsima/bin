#! /usr/bin/env sh
# Add org file changes to the repository
#
# source: http://doc.norang.ca/org-mode.html#GitSync

cd ~/org

# Remove deleted files
git ls-files --deleted -z | xargs -0 git rm >/dev/null 2>&1

# Add new files
git add . >/dev/null 2>&1

git commit -m "$(date +%Y.%m.%d..%H.%M)"
