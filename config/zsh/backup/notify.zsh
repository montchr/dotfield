#!/usr/bin/env zsh
#
# znotify
#
# This script triggers a desktop notification when a command finishes if
# it has been running for longer than $zbell_duration seconds.
#
# If there are programs that you know run long that you don't
# want to bell after, then add them to $zbell_ignore.
#
# Based off the original work of Jean-Philippe Ouellet
# https://gist.github.com/jpouellet/5278239
#

# only works with kitty terminal
# (or other terminals supporting OSC 99 desktop notifications)
# [[ "$TERM" == "xterm-kitty" ]] || return

# only do this if we're in an interactive shell
[[ -o interactive ]] || return

# get $EPOCHSECONDS. builtins are faster than date(1)
zmodload zsh/datetime || return

# make sure we can register hooks
autoload -Uz add-zsh-hook || return

# initialize zbell_duration if not set
(( ${+zbell_duration} )) || zbell_duration=5

# initialize zbell_ignore if not set
(( ${+zbell_ignore} )) || zbell_ignore=($EDITOR $PAGER)

# initialize it because otherwise we compare a date and an empty string
# the first time we see the prompt. it's fine to have lastcmd empty on the
# initial run because it evaluates to an empty string, and splitting an
# empty string just results in an empty array.
zbell_timestamp=$EPOCHSECONDS

# right before we begin to execute something, store the time it started at
zbell_begin() {
  zbell_timestamp=$EPOCHSECONDS
  zbell_lastcmd=$1
}

# when it finishes, if it's been running longer than $zbell_duration,
# and we dont have an ignored command in the line, then print a bell.
zbell_end() {
  exit_code=$?
  ran_long=$(( $EPOCHSECONDS - $zbell_timestamp >= $zbell_duration ))

  has_ignored_cmd=0
  for cmd in ${(s:;:)zbell_lastcmd//|/;}; do
    words=(${(z)cmd})
    util=${words[1]}
    if (( ${zbell_ignore[(i)$util]} <= ${#zbell_ignore} )); then
      has_ignored_cmd=1
      break
    fi
  done

  if (( ! $has_ignored_cmd )) && (( ran_long )); then
    if [[ $exit_code ]]; then
      printf '\x1b]99;i=1:d=0;Done in %d seconds.\x1b\\' $zbell_duration
      # printf '\x1b]99;;Done in %d seconds.\x1b\\' $zbell_duration
    else
      # printf '\x1b]99;;Command exited with status %d after %d seconds.\x1b\\' \
      #   $exit_code $zbell_duration
      printf '\x1b]99;i=1:d=0;Command exited with status %d after %d seconds.\x1b\\' \
        $exit_code $zbell_duration
      print -n "\a"
    fi
    printf '\x1b]99;i=1:d=1:p=body;%s\x1b\\' $zbell_lastcmd
  fi
}

# register the functions as hooks
add-zsh-hook preexec zbell_begin
add-zsh-hook precmd zbell_end
