#!/usr/bin/env bats

BASE="${BATS_TEST_DIRNAME}/.."

load "${BASE}/utils.sh"
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

@test "[create]: Label must include prefix if running in CI" {
  CI=true

  run main create "foo"
  assert_output -p "Linode label 'foo' does not begin with"
  assert_failure

  run main create "foo-ci-dots-"
  assert_output -p "Linode label 'foo-ci-dots-' does not begin with"
  assert_failure

  run main create "ci-dots-main"
  assert_output -p "Linode label 'ci-dots-main' does not include an image"
  assert_failure
}

@test "[create]: Label prefix in CI must not be the entire label" {
  CI=true

  run main create "ci-dots-"
  assert_output -p "Linode label 'ci-dots-' does not have enough information"
  assert_failure
}
