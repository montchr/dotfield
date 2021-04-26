# -*- mode: sh; eval: (sh-set-shell "bash") -*-
# shellcheck shell=bash
#
# OS :: macOS :: Pre-Installation Requirements
#


# shellcheck source=../../lib/utils.sh
. "${XDG_CONFIG_HOME}/lib/utils.sh"


function main () {

  guard::install && {
    msg::subdomain "Ensure Homebrew exists" && {
      shell::has brew || {
        msg::info "Installing brew"
        /bin/bash -c "printf \"\n\" | $(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        brew update
      }
    }

    msg::subdomain "Ensure a recent version of Bash and ZSH exist" && {
      brew install bash
      brew install zsh
    }


    msg::subdomain "Allow Homebrew's Bash and ZSH as login shells" && {
      local brew_bash_path
      brew_bash_path="$(brew --prefix)/bin/bash"

      # Add the path of the Bash version installed through Homebrew to the list
      # of allowed login shells in the `/etc/shells` file.
      if ! grep "${brew_bash_path}" < /etc/shells &> /dev/null; then
        printf '%s\n' "${brew_bash_path}" | sudo tee -a /etc/shells
      fi
    }
  }


}

main "$@"
