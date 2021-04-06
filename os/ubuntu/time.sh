#!/usr/bin/env bash
#
# os/ubuntu/time
#
# Setup and configure time services.
#

# shellcheck source=../../lib/utils.sh
. "${DOTFILES_DIR}/lib/utils.sh"

# Setup timezone.
# Parameters:
#   Timezone
function time::setup_timezone () {
  local timezone
  timezone="$1"

  if [[ -z "${timezone}" ]]; then
    ask "Enter the timezone for the server (Default is 'America/New_York')"
    timezone=$(get_answer)
  fi

  timezone=${timezone:-America/New_York}

  # Set the timezone.
  echo "${timezone}" | sudo tee /etc/timezone
  sudo ln -fs \
    "/usr/share/zoneinfo/${timezone}" \
    /etc/localtime # https://bugs.launchpad.net/ubuntu/+source/tzdata/+bug/1554806
  sudo dpkg-reconfigure -f noninteractive tzdata
}

# Setup NTP.
function time::setup_ntp () {
  local ubuntu_version
  ubuntu_version="$(lsb_release -sr)"

  if [[ $ubuntu_version == '20.04' ]]; then
    sudo systemctl restart systemd-timesyncd
  else
    sudo apt-get update
    sudo apt-get --assume-yes install ntp
  fi
}

function main () {
  
}

main "$@"
