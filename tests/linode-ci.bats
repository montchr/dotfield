#!/usr/bin/env bats

load ../lib/utils.sh
load ../vendor/bats-support/load
load ../vendor/bats-assert/load
load ../vendor/bats-file/load


main() {
  bash "${BATS_TEST_DIRNAME}/../.ci/linode-ci.sh" "$@"
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
