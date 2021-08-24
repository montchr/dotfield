#!/usr/bin/env bash
#
# Provision :: Nix Package Manager
# 

#========================================
# Install Nix package manager.
#========================================
function __install() {
  if guard::macos; then
    # Use the new installer from cachix until it becomes available in Nix 2.4
    # This fixes many difficulties with the installer for Nix on Darwin, and
    # finally allows for an encrypted Nix store.
    #
    # TODO: Alternatively, use the installer from Nix master branch.
    #
    # https://github.com/NixOS/nix/pull/4289
    sh <(curl https://abathur-nix-install-tests.cachix.org/serve/yihf8zbs0jwph2rs9qfh80dnilijxdi2/install) \
      --tarball-url-prefix https://abathur-nix-install-tests.cachix.org/serve
  else
    sh <(curl -L https://nixos.org/nix/install) --daemon
  fi

}


#========================================
# Install nix-darwin.
#========================================
function __install_nix_darwin() {
  [[ -d ./result ]] && {
    msg::subdomain "Found existing ./result directory. Removing..."
    rm -rf ./result
  }

  # Although nix-darwin supports flakes, at the time of writing, the installer
  # does not. In order to install nix-darwin properly, it needs to be
  # installed for the first time via the installer.
  # See: https://github.com/LnL7/nix-darwin#flakes-experimental
  msg::subdomain "Building nix-darwin installer"

  if ! nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer; then

    msg::error "Encountered an error installing nix-darwin"
    cert_file="/etc/ssl/certs/ca-certificates.crt"

    msg::stream::info \
<<END

If you received a message like this:

> Problem with the SSL CA cert (path? access rights?) (77)

...then '${cert_file}' might be a dead symlink.

Try removing and re-linking it by running the following:

$ sudo rm ${cert_file}
$ sudo ln -s /nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt ${cert_file}

END

    exit 1

  fi

  msg::subdomain "Running nix-darwin installer"
  ./result/bin/darwin-installer

  msg::subdomain "Sourcing '/etc/static/bashrc' for nix-darwin additions"
  . /etc/static/bashrc

}


#========================================
# Enable experimental flakes support
#========================================
function __flakify() {
  nix-env -iA nixpkgs.nixFlakes

  nix_flakes_feature_flag="experimental-features = nix-command flakes"

  if ! grep --quiet --fixed-strings "${nix_flakes_feature_flag}" /etc/nix/nix.conf; then
    msg::subdomain "Adding nix flakes feature flag to /etc/nix/nix.conf"
    echo "${nix_flakes_feature_flag}" | sudo tee -a /etc/nix/nix.conf
  fi
}



#========================================
# Main entry.
#========================================
function main() {
  ( [[ -d /nix ]] || ! guard::nixified ) && {
    msg::domain "Nix" "Install Nix package manager"
    __install

    msg::warning "Nix was installed, but you need to run the provisioning script again in a new session before continuing! Skipping this time around..."
    return 0
  }

  msg::subdomain "Testing nix-shell"
  nix-shell -p nix-info --run "nix-info -m"

  msg::domain "Nix" "Enable experimental Nix Flakes" && {
    __flakify
  }

  guard::macos && ! shell::has darwin-help && {
    msg::domain "Nix" "Install nix-darwin"
    __install_nix_darwin
  }

  return 0
}

main "$@"
