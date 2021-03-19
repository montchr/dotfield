#!/usr/bin/env bash
#
# os/ubuntu/lib/swapfile
#
# Helper functions for swapfile setup.
#

# shellcheck source=../../../lib/utils.sh
. "${DOTFILES_DIR}/lib/utils.sh"


# Get the amount of installed physical memory in GB (rounded up).
function swap::get_mem () {
  local phymem
  phymem="$(free -g|awk '/^Mem:/{print $2}')"

  [[ ${phymem} == '0' ]] && \
    exit 1

  echo "${phymem}"
}

# Create swapfile.
#
# Uses
#   swap::get_mem
#
function swap::create () {
  local swapmem=$(($(.swap.get_mem) * 2))

  # Anything over 4GB in swap is probably unnecessary as a RAM fallback
  if [ ${swapmem} -gt 4 ]; then
      swapmem=4
  fi

  sudo fallocate -l "${swapmem}G" /swapfile
  sudo chmod 600 /swapfile
  sudo mkswap /swapfile
  sudo swapon /swapfile
}

# Mount the swapfile.
function swap::mount () {
  sudo cp /etc/fstab /etc/fstab.bak
  echo '/swapfile none swap sw 0 0' | sudo tee -a /etc/fstab
}

# Adjust the swapfile settings.
# Parameters:
#   Runtime or permanent -- can be `run` or `save`
#   Swappiness value
#   VFS cache pressure value
function swap::adjust () {
  local action=${1}
  local swappiness=${2}
  local vfs_cache_pressure=${3}

  case $action in
    run)
      sudo sysctl vm.swappiness="${swappiness}"
      sudo sysctl vm.vfs_cache_pressure="${vfs_cache_pressure}"
      ;;
    save)
      echo "vm.swappiness=${swappiness}" | sudo tee -a /etc/sysctl.d/50-swapfile.conf
      echo "vm.vfs_cache_pressure=${vfs_cache_pressure}" | sudo tee -a /etc/sysctl.d/50-swapfile.conf
      ;;
    *)
      print_error "Unknown action!"
      exit 1
      ;;
  esac
}
