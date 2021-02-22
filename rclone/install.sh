#!/usr/bin/env bash
set -euo pipefail

# @TODO use `has()` function -- but it's unavailable to non-login shells
if [[ ! $(type rclone >/dev/null 2>&1) ]]; then
  cd "$(mktemp -d -t rclone)" || exit 1
  curl -O https://downloads.rclone.org/rclone-current-osx-amd64.zip
  unzip -a rclone-current-osx-amd64.zip && cd rclone-*-osx-amd64
  mv rclone /usr/local/bin/
fi

if [ ! -d "$HOME/.mount" ]; then mkdir ~/.mount; fi

cd "$HOME/.mount"

mounts=("alley-gdrive" "gdrive" "silo")

for mount in "${mounts[@]}"; do
	if [ ! -d "$mount" ]; then mkdir "$mount"; fi
done
