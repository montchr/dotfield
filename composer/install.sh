#!/usr/bin/env bash
#
# composer/install
#

function main {
  local EXPECTED_CHECKSUM
  local ACTUAL_CHECKSUM
  local RESULT

  EXPECTED_CHECKSUM="$(php -r 'copy("https://composer.github.io/installer.sig", "php://stdout");')"
  php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');"
  ACTUAL_CHECKSUM="$(php -r "echo hash_file('sha384', 'composer-setup.php');")"

  if [ "$EXPECTED_CHECKSUM" != "$ACTUAL_CHECKSUM" ]
  then
    >&2 echo 'ERROR: Invalid installer checksum'
    rm composer-setup.php
    return 1
  fi

  php composer-setup.php \
    --install-dir="${XDG_BIN_HOME}" \
    --filename="composer"
  RESULT=$?
  rm composer-setup.php
  return $RESULT
}

main "$@"