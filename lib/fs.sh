# -*- mode: sh; eval: (sh-set-shell "bash") -*-
#
# Shell Utilities :: Filesystem
#

[[ ${Utils[fs]} ]] \
  && return \
  || Utils[fs]=${BASH_SOURCE[0]:-${(%):-%x}}



function fs::ensure_dir {
  if [[ ! -d "$1" ]]; then
    msg::info "create $1"
    mkdir -p "$1"
  fi
  msg::error $? "$1"
}

function fs::linkfile {
  local file="$1"
  if [ -f "$file" ]; then
    (
      cd "$(dirname "$file")" || return 1
      fs::map_lines fs::safe_link "$file"
    )
  fi
}

# @TODO untested
function fs::safe_link_all {
  local src_dir="$1"
  local target_dir="$2"
  
  if ! [[ -d "${src_dir}" ]]; then
    msg::error "[Error] Source '${src_dir}' is not a directory! Aborting."
  fi
  if ! [[ -d "${target_dir}" ]]; then
    msg::error "[Error] Target '${target_dir}' is not a directory! Aborting."
  fi

  msg::warning "Linking all files in '${src_dir}' to '${target_dir}':"
  for f in "${src_dir}/*"; do
    msg::info "$f"
  done

  for f in "${src_dir}/*"; do
    fs::safe_link "$f" "${target_dir}/$(basename "$f")"
  done
}

function fs::safe_link {
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

function fs::map_lines {
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
function fs::download_bin {
  fp="$HOME/.local/bin/$1"
  curl --silent -o "$fp" "$2"
  chmod a+x "$HOME/.local/bin/$1"
  hash -r
}
