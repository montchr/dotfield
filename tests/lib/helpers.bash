#!/usr/bin/env bash
#
# bash testing helpers
#


# Load a Bats library or project utility.
#
# Globals:
#   none
# Arguments:
#   $1 - Context
#   $2 - Helper name(s)...
# Returns:
#   0 - on success
#   1 - otherwise
function tests::import() {
  local context="$1"
  shift
  local names="$*"
  for file in $names; do
    case "${context}" in
      bats) load "../vendor/bats-${file}/load.bash" ;;
      util) load "../lib/${file}" ;;
      *)
        echo "Unsupported context! Aborting."
        return 1
        ;;
    esac
  done
}
