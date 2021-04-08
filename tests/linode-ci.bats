#!/usr/bin/env bats

BASE="${BATS_TEST_DIRNAME}/.."

load "${BASE}/lib/utils.sh"
load "${BASE}/vendor/bats-support/load.bash"
load "${BASE}/vendor/bats-assert/load.bash"
load "${BASE}/vendor/bats-file/load.bash"


main() {
  bash "${BASE}/.ci/linode-ci.sh" "$@"
}

@test "args are valid" {
  run main
  assert_failure
  assert_output -p "No action specified! Aborting."
}

@test "invalid actions are not allowed" {
  run main foo
  assert_failure
  assert_output -p "Invalid action 'foo' passed! Aborting."
}


@test "linode-cli must be available" {
  run main foo
  refute_output -p "linode-cli not found!"
}
