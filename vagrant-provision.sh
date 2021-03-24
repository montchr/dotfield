#!/usr/bin/env bash

set -e

# @TODO how to pass the required env vars to the other users?
function .do_bootstrap() {
  local user="$1"
  sudo su - "${user}" -c "
    cd ${HOME}
    cp /vagrant/bootstrap ${HOME}/bootstrap
    bash -c ${HOME}/bootstrap
  "
}

.do_bootstrap root
.do_bootstrap "${CDOM_INIT_NEW_USER_NAME}"
