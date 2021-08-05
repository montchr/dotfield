#!/usr/bin/env bats
#
# tests for The World
#

BASE="${BATS_TEST_DIRNAME}/.."

# FIXME: update in https://github.com/montchr/dotfield/issues/21
load "${BASE}/dotfield/lib/utils.sh"
load "${BASE}/vendor/bats-support/load.bash"
load "${BASE}/vendor/bats-assert/load.bash"
load "${BASE}/vendor/bats-file/load.bash"

@test "Ensure variables are set to expected defaults" {
  [[ "$(whoami)" == "${USER}" ]]
}

@test "Verify macOS variables" {
  [[ "darwin" == "${KERNEL_NAME}" ]]
  [[ "${OS_VERSION}" == 11.* ]]
}
