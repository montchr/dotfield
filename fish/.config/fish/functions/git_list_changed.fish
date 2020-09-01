function git_list_changed -d "List files changed from primary Git branch"
  git diff --name-only --diff-filter=ACMRTUXB $GIT_PRIMARY_BRANCH HEAD
end
