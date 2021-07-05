#!/usr/bin/env bash
#
# ci-linode-up
#
# Create a new Linode for CI purposes.
#

readonly BASE_DIR="$( cd "${BASH_SOURCE[0]%/*}/.." && pwd )"

# shellcheck source=../utils.sh
. "${BASE_DIR}/utils.sh"

# Repository name without a user prefix.
REPO="${REPO:-${GITHUB_REPOSITORY#${GITHUB_USER}/}:-dotfield}"
# Branch for the linode.
BRANCH="${GITHUB_REF:-${GIT_BRANCH_NAME:-main}}"
# Root password for the linode.
PASSWORD="${LINODE_ROOT_PASSWORD:-}"
# Source image ID.
LINODE_IMAGE_ID="${LINODE_IMAGE_ID:-linode/debian10}"
# Label for the linode based on the repo name, branch, and unprefixed image ID.
readonly LINODE_LABEL="ci-${REPO}-${BRANCH}-${LINODE_IMAGE_ID#linode/}"


#========================================
# Wrapper for running a linode-cli command with machine-readable output.
#
# Usage:
#   linode.run <args>...
#========================================
function linode.run {
  linode-cli --no-headers --text "$*"
}


#========================================
# Get a field from a Linode resource.
#
# Usage:
#   linode.get_field <resource-type> <resource-label> <field-key>
#========================================
function linode.get_field {
  local type=$1
  local label="$2"
  local key="$3"
  linode.run "${type}s" list \
    --format="${key}" \
    --label="${label}"
}


#========================================
# Whether a linode with the specified label is rebuilding.
#
# Usage:
#   linode.is_rebuilding <linode-label>
#========================================
function linode.is_rebuilding() {
  local label=$1
  [[ "rebuilding" == $(linode.get_field linode "${label}" status) ]]
}


#========================================
# Whether a linode with the specified label is running.
#
# Usage:
#   linode.is_running <linode-label>
#========================================
function linode.is_running() {
  local label=$1
  [[ "running" == $(linode.get_field linode "${label}" status) ]]
}


#========================================
# Print a human-friendly table of info about the specified linode.
#
# Usage:
#   linode.print_info <linode-label>
#========================================
function linode.print_info() {
  local label=$1
  linode-cli linodes list --label="${label}"
}


#========================================
# Monitor a linode for an expected status.
#
# Usage:
#   linode.check_status <linode-label> <expected-status>
#========================================
function linode.check_status() {
  local label=$1
  local expected_status=$2
  local pause=10

  until "linode.is_${expected_status}" "${label}"; do
    [[ pause -eq 0 ]] && {
      print_error "[Error]" "Status check timed out. Aborting."
      print_warning "You may want to verify the status manually."
      return 1
    }

    # The Linode CLI doesn't appear to report the linode's status immediately,
    # so decrease the wait time in an attempt to catch the change in status.
    execute "sleep ${pause}" "Waiting for '${expected_status}' status (${pause}s)" \
      && pause=$(( pause - 2 ))
  done

  print_success "Linode is ${expected_status}!"
}


#========================================
# Create a linode with the specified label.
#
# Usage:
#   linode.create <linode-label>
#========================================
function linode.create() {
  local label=$1

  if [[ -n "${label}" ]]; then
    if is_ci && [[ "${label}" != "ci-dotfield-"* ]]; then
      print_error "[ERROR] Linode label '${label}' does not begin with 'ci-dotfield-'. Aborting."
      return 1
    fi

    # @TODO validate label name
    # must not have two consecutive separators!
    # https://www.linode.com/docs/api/linode-instances/#linode-create

  else
    print_error "[ERROR] You need to specify a source image in order to create a new linode! Aborting."
    return 1
  fi

  [[ -z "${LINODE_IMAGE_ID}" ]] && {
    print_error "[ERROR] You need to specify a source image in order to create a new linode! Aborting."
    return 1
  }

  # @TODO validate image name
  # @TODO what does the linode-cli do if an invalid image name is passed?

  [[ -z "${PASSWORD}" ]] && {
    print_error "[ERROR] Linode root password not specified! Aborting."
    return 1
  }

  exit

  linode.run linodes create \
    --type=g6-nanode-1 \
    --region=us-east \
    --backups_enabled=false \
    --image="${LINODE_IMAGE_ID}" \
    --root_pass="${PASSWORD}" \
    --booted=true \
    --label="${label}" \
    --tags=ci \
    --tags=github-actions
}


#========================================
# Destroy a linode.
#
# Usage:
#   linode.destroy <linode-label>
#========================================
function linode.destroy() {
  local label=$1
  local id
  id="$(linode.get_field linode "${label}" id)"
  linode.run linodes destroy "${id}"
}


#========================================
# Rebuild a linode from a source image.
#
# Usage:
#   linode.rebuild <linode-label>
#========================================
function rebuild() {
  local linode_label

  linode.is_rebuilding "${linode_label}" && {
    linode.print_info "${linode_label}"
    print_warning "The linode '${linode_label}' is already rebuilding!" "Aborting."
    return 1
  }

  if [[ -z "${PASSWORD}" ]]; then
    if is_ci; then
      print_error "Root password is not set! Aborting."
      return 1
    fi
    set_password_global
  fi

  linode.print_info "${linode_label}"

  print_warning "The CI linode will be destroyed and rebuilt!"

  if ! is_ci; then
    ask_for_confirmation "Do you want to continue?"

    if ! user_confirmed; then
      print_error "Cancelled!" "Exiting..."
      return 1
    fi
  fi

  linode_id=$(linode.get_field linode "${linode_label}" id)

  linode-cli linodes rebuild "${linode_id}" \
    --image="${LINODE_IMAGE_ID}" \
    --root_pass="${PASSWORD}"
  print_result $? "Initiated linode rebuild"

  linode.check_status "${linode_label}"
  linode.print_info "${linode_label}"
}

function main() {
  local action="$1"
  local linode_label="${2:-LINODE_LABEL}"

  [[ -z "${action}" ]] && {
    print_error "No action specified! Aborting."
    return 1
  }

  if ! cmd_exists "linode-cli"; then
    print_error "[Error]" "linode-cli not found!"
    return 1
  fi

  case $action in
    create) create "${linode_label}" ;;
    destroy) destroy "${linode_label}" ;;
    rebuild) rebuild "${linode_label}" ;;
    *)
      print_error "[Error] Invalid action '${action}' passed! Aborting."
      return 1 ;;
  esac

}

main "$@"
