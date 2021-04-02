# -*- mode: sh; eval: (sh-set-shell "bash") -*-
#
# Shell Utilities :: Repositories
#

[[ ${Utils[repo]} ]] \
  && return \
  || Utils[repo]=${BASH_SOURCE[0]:-${(%):-%x}}

. ./msg.sh

function repo::is_repo() {
  git rev-parse &>/dev/null
}

function repo::qualify_url() {
  if [[ "$1" = "https://"* || "$1" = "git@"* ]]; then
    echo "$1"
  elif [[ "$2" = "github" ]]; then
    if [[ "$USE_HTTPS" = "true" ]]; then
      echo  "https://github.com/$1.git"
    else
      echo "git@github.com:$1.git"
    fi
  elif [[ "$2" = "gitlab" ]]; then
    if [[ "$USE_HTTPS" = "true" ]]; then
      echo  "https://gitlab.com/$1.git"
    else
      echo "git@gitlab.com:$1.git"
    fi
  fi
}

function repo::log() {
  git --no-pager \
      log \
      --graph \
      --pretty=format:'%Cred%h%Creset %C(bold blue)<%an> -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' \
      "$*"
}

function repo::sync() {
  local dir=$1
  local remote=$2
  local url=$3
  local branch=$4

  msg::section "sync_repo $*"

  local remote_branch="${remote_branch}"

  wd=$(eval echo "${dir}")
  url=$(qualify_repo_url "${url}" "$remote")
  if [[ $branch = "" ]]; then
    branch="main"
  fi

  if [[ -d "$wd/.git" ]]; then
    msg::info "$wd already exists"
  else
    git clone "$url" "$wd" -b "$branch"
  fi

  cd "$wd" && {
    git diff-index --quiet HEAD -- || {
      msg::error "Your working directory is not clean."
      msg::error "Please commit or stash all changes before proceeding."
      return 1
    }

    current_branch=$(git symbolic-ref --short HEAD)
    if [[ $branch != "$current_branch" ]]; then
      msg::info "Switching from $current_branch to $branch"
      git checkout "$branch"
    fi

    if [[ -d .git/refs/remotes/$remote ]]; then
      current_url=$(git remote get-url "$remote")
      if [[ $current_url != "$url" ]]; then
        msg::info "Remote '$remote' has wrong url, so updating it"
        msg::info "  $current_url -> $url"
        git remote set-url "$remote" "$url"
      fi
    else
      msg::warning "Could not find remote '$remote', so adding it"
      git remote add "${remote}" "${url}"
    fi

    msg::info "fetch ${remote}"
    git fetch "${remote}"
    if [[ $(git rev-parse HEAD) == $(git rev-parse "${remote_branch}") ]]; then
      msg::success "Everything up-to-date"
      return 0
    fi

    if [ "$(git rev-list "HEAD..${remote_branch}" --count)" != 0 ]; then
      msg::info "Fetched changes:"
      repo::log "HEAD..${remote_branch}"
      msg::info
    fi

    msg::info "rebase onto ${remote_branch}"
    git rebase "${remote_branch}"

    if [[ "$url" = *"${FELLOW}"* ]]; then
      if [ "$(git rev-list "${remote_branch}..HEAD" --count)" != 0 ]; then
        msg::info "Changes to push:"
        repo::log "${remote_branch}..HEAD"
        msg::info
      fi

      msg::info "pushing changes"
      git push "${remote}" "${branch}"
    fi
  }
}
