#!/usr/bin/env bats

BASE="${BATS_TEST_DIRNAME}/../.."

load "${BASE}/lib/utils.sh"
load "${BASE}/vendor/bats-support/load.bash"
load "${BASE}/vendor/bats-assert/load.bash"
load "${BASE}/vendor/bats-file/load.bash"

main() {
  string::sanitize "$@"
}

@test "Replace slashes with underscores" {
  run main "abcd/efgh//ijkl/"
  assert_output "abcd_efgh__ijkl_"
}
