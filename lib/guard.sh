# -*- mode: sh; eval: (sh-set-shell "bash") -*-
#
# Shell Utilities :: Guard
#
# https://d12frosted.io/posts/2018-11-04-revisiting-eru.html#dealing-with-multiple-optional-themes
#

[[ ${Utils[guard]} ]] \
  && return \
  || Utils[guard]=${BASH_SOURCE[0]:-${(%):-%x}}


. ./00-world.sh
. ./msg.sh


function guard::domain() {
  local domain=$1
  key=$(echo "${domain}" | tr '[:upper:]' '[:lower:]')
  shift

  local message=$*

  local guard_ref="guard_$key"
  local ignore_guard_ref="guard_ignore_$key"
  guard="${!guard_ref}"
  ignore_guard="${!ignore_guard_ref}"

  if [[ ("$ALL" == "true" || "$guard" == "true") && "$ignore_guard" == "" ]]; then
    msg::domain__lesser "${domain}" "${message}"
    return 0
  else
    msg::domain__inactive "${domain}" "${message}"
    return 1
  fi
}

function guard::install() {
  [[ "$ACTION" == "install" ]]
  return
}

function guard::upgrade() {
  [[ "$ACTION" == "upgrade" ]]
  return
}

function guard::test() {
  [[ "$ACTION" == "test" ]]
  return
}

function guard::ubuntu() {
  [[ "$OS_NAME" == "ubuntu" ]]
  return
}

function guard::arch() {
  [[ "$OS_NAME" == "arch" ]]
  return
}

function guard::macos() {
  [[ "$OS_NAME" == "macos" ]]
  return
}

function guard::user() {
  [[ $(whoami) == "$1" ]]
  return
}
