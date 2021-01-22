#!/usr/bin/env bash
set -euo pipefail

if [ ! -d "$HOME/.mount" ]; then mkdir ~/.mount; fi

cd "$HOME/.mount"

mounts=("alley-gdrive" "gdrive" "silo")
for mount in "${mounts[@]}"; do
	if [ ! -d "$mount" ]; then mkdir "$mount"; fi
done
