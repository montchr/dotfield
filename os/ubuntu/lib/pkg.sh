#!/usr/bin/env bash
#
# os/ubuntu/lib/pkg
#
# Utilities for working with packages.
#

# shellcheck source=../../../lib/utils.sh
. "${DOTFIELD_UTILS_DIR}/utils.sh"


# Resynchronize the package index files from their sources.
function pkg::update_repos () {
  execute \
    "sudo apt-get update -qqy" \
    "APT (update)"
}

# Install the newest versions of all installed packages.
function pkg::upgrade_all() {
  execute \
    "export DEBIAN_FRONTEND=\"noninteractive\" \
      && sudo apt-get -o Dpkg::Options::=\"--force-confnew\" upgrade -qqy" \
    "APT (upgrade)"
}

# Install a package.
# Parameters:
#   Package description
#   Package name
#   Extra arguments to apt-get
function pkg::install () {
  DESCRIPTION="$1"
  PACKAGE="$2"
  EXTRA_ARGUMENTS="$3"

  if ! pkg::exists "${PACKAGE}"; then
    execute \
      "sudo apt-get install --allow-unauthenticated -qqy ${EXTRA_ARGUMENTS} ${PACKAGE}" \
      "${DESCRIPTION}"
  else
    print_success "${DESCRIPTION}"
  fi
}

# Whether a package is installed.
# Parameters:
#   Package name
function pkg::exists () {
  dpkg -s "$1" &> /dev/null
}
