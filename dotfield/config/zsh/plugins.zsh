ZGEN_LOADED=()
ZGEN_COMPLETIONS=()

zgenom oh-my-zsh

zgenom load zsh-users/zsh-history-substring-search

# Set keystrokes for substring searching
zmodload zsh/terminfo

zgenom load unixorn/autoupdate-zgenom

# Colorize command output.
zgenom load unixorn/warhol.plugin.zsh

# TODO: this plugin might not load efficiently. see old zshrc for prior art.
zgenom load chriskempson/base16-shell

# @unixorn's macOS helpers.
zgenom load unixorn/tumult.plugin.zsh

zgenom load djui/alias-tips
zgenom load unixorn/git-extra-commands
zgenom load skx/sysadmin-util

# Aliases for working with current repo on GitHub.
zgenom load peterhurford/git-it-on.zsh

# Encrypt some repo files.
zgenom load StackExchange/blackbox

# Load some oh-my-zsh plugins
zgenom oh-my-zsh plugins/aws
zgenom oh-my-zsh plugins/colored-man-pages
zgenom oh-my-zsh plugins/command-not-found
zgenom oh-my-zsh plugins/composer
zgenom oh-my-zsh plugins/direnv
zgenom oh-my-zsh plugins/dotenv
zgenom oh-my-zsh plugins/fd
zgenom oh-my-zsh plugins/git
zgenom oh-my-zsh plugins/github
# TODO: prob requires configuration
# zgenom oh-my-zsh plugins/jira
zgenom oh-my-zsh plugins/npm
# zgenom oh-my-zsh plugins/pass
zgenom oh-my-zsh plugins/pip
zgenom oh-my-zsh plugins/python
zgenom oh-my-zsh plugins/rsync
zgenom oh-my-zsh plugins/screen
zgenom oh-my-zsh plugins/sudo
zgenom oh-my-zsh plugins/vagrant
zgenom oh-my-zsh plugins/wd
zgenom oh-my-zsh plugins/wp-cli

if [[ $(uname -a | grep -ci Darwin) == 1 ]]; then
  # Load macOS-specific plugins
  zgenom oh-my-zsh plugins/brew
  zgenom oh-my-zsh plugins/osx
fi

# TODO: barely does anything -- why not just copy?
# zgenom load chrissicool/zsh-256color

zgen load hlissner/zsh-autopair \
  autopair.zsh

zgenom load zsh-users/zsh-completions \
  src

# fzf completion
zgen load junegunn/fzf \
  shell

# zgenom load srijanshetty/docker-zsh

zgenom load RobSis/zsh-completion-generator

zgenom load zsh-users/zsh-autosuggestions

[[ -z "$SSH_CONNECTION" ]] && {
  zgen load zdharma/fast-syntax-highlighting
}

zgenom load softmoth/zsh-vim-mode

zgenom load romkatv/powerlevel10k \
  powerlevel10k

zgenom save
