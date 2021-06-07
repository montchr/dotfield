#!/usr/bin/env bats
#
# tests for str::sanitize
#

BASE="${BATS_TEST_DIRNAME}/../.."

load "${BASE}/utils.sh"
load "${BASE}/vendor/bats-support/load.bash"
load "${BASE}/vendor/bats-assert/load.bash"
load "${BASE}/vendor/bats-file/load.bash"

main() {
  str::sanitize "$@"
}

@test "Replace spaces with dashes" {
  run main " foo  bar "
  assert_output "-foo--bar-"
}

@test "Replace slashes with underscores" {
  run main '/f/o//o/'
  assert_output "_f_o__o_"
}

@test "Ensure string is lowercased" {
  run main "FOO"
  assert_output "foo"
}

@test "Ensure valid characters are retained" {
  run main 'a1B0._-'
  assert_output 'a1b0._-'
}

@test "Ensure ignored characters are stripped" {
  # shellcheck disable=2016
  run main 'foo$($=\\``)bar'
  assert_output "foobar"
}
