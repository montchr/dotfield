#!/usr/bin/env bash
#
# ci-linode-up
#
# Create a new Linode for CI purposes.
#

# LINODE_ROOT_PASSWORD="$1"
declare -g LINODE_LABEL

function .sanitize_label() {
  # Replace slashes with underscores
  echo "${1//\//_}"
}


function main() {
  linode-cli linodes create \
    --type=g6-nanode-1 \
    --region=us-east \
    --backups_enabled=false \
    --image=linode/ubuntu20.04 \
    --root_pass="${LINODE_ROOT_PASSWORD}" \
    --booted=true \
    --label='ci-dots-feature_linode_ci' \
    --tags=ci --tags=github-actions
}

main "$@"
