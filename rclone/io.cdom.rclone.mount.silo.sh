#!/bin/sh

/usr/local/bin/rclone mount \
	--transfers 8 \
	--volname Silo \
	--allow-other \
	--log-level INFO \
	--buffer-size 32M \
	--vfs-cache-mode writes \
	--vfs-cache-max-size 8G \
	silo:/ /Users/chrismontgomery/.mount/silo
