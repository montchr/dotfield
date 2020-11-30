#!/bin/sh

/usr/local/bin/rclone cmount \
	--rc \
	--transfers 8 \
	--volname CDOM \
	--allow-other \
	--log-level INFO \
	--buffer-size 32M \
	--vfs-cache-mode writes \
	--vfs-cache-max-size 8G \
	gdrive:/ /Users/chrismontgomery/.mount/gdrive
