#!/bin/sh

/usr/local/bin/rclone mount \
	--transfers 8 \
	--volname Alley \
	--allow-other \
	--log-level INFO \
	--buffer-size 32M \
	--vfs-cache-mode writes \
	--vfs-cache-max-size 8G \
	alley-gdrive:/ /Users/chrismontgomery/.mount/alley-gdrive
