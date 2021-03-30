# -*- mode: sh; eval: (sh-set-shell "bash") -*-
#
# Shell Utilities :: Filesystem
#

function check() {
  command -v "$1" >/dev/null 2>&1
}

cmd_exists() {
  command -v "$1" &>/dev/null
}

function ensure_dir() {
  if [[ ! -d "$1" ]]; then
    msg::info "create $1"
    mkdir -p "$1"
  fi
}

function linkfile() {
  local file="$1"
  if [ -f "$file" ]; then
    (
      cd "$(dirname "$file")" || return 1
      map_lines safe_link "$file"
    )
  fi
}

function safe_link() {
  local f
  local s
  local t
  local d
  local owner

  # shellcheck disable=SC2086
  f=$(eval echo $1)
  s="$(pwd)/$f"
  t=$(eval echo "$2")
  d=$(dirname "$t")

  if [[ -d "$d" ]]; then
    owner=$(stat -c '%U' "$d")
    if [[ "$owner" != "root" && "$owner" != "$USER" ]]; then
      msg::error "can not link '$s' to '$t'"
      msg::error "owner of '$d' is $owner"
      msg::error "allowed owners: root or $USER"
      exit 1
    fi
  fi

  if [[ ! -f "$s" && ! -d "$s" ]]; then
    msg::error "can not link '$s' as it does not exist"
    exit 1
  fi

  if [[ ! -d $d ]]; then
    msg::info "create $d"
    mkdir -p "$d"
  fi

  if [[ -L "$t" ]]; then
    msg::info "relink $s -> $t"
    if [[ "$owner" = "root" ]]; then
      sudo rm "$t"
    else
      rm "$t"
    fi
  else
    msg::info "link $s -> $t"
  fi

  if [[ "$owner" = "root" ]]; then
    sudo ln -s "$s" "$t"
  else
    ln -s "$s" "$t"
  fi
}

function map_lines() {
  if [[ -f "$2" ]]; then
    while IFS='' read -r line || [[ -n "$line" ]]; do
      if [[ "$line" != "#"* ]]; then
        # shellcheck disable=SC2086
        $1 $line
      fi
    done < "$2"
  fi
}

# @TODO might not belong in this file
function download_bin() {
  fp="$HOME/.local/bin/$1"
  curl --silent -o "$fp" "$2"
  chmod a+x "$HOME/.local/bin/$1"
  hash -r
}
