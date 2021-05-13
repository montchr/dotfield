ZGEN_LOADED=()
ZGEN_COMPLETIONS=()

zgenom oh-my-zsh


# Order matters here!
# 1. Aloxaf/fzf-tab
# 2. zsh-users/zsh-syntax-highlighting
# 3. zsh-users/zsh-history-substring-search
zgenom load Aloxaf/fzf-tab
zgenom load zsh-users/zsh-syntax-highlighting
zgenom load zsh-users/zsh-history-substring-search

# Set keystrokes for substring searching
zmodload zsh/terminfo

zgenom load unixorn/autoupdate-zgenom

# Colorize command output.
zgenom load unixorn/warhol.plugin.zsh

# TODO: this plugin doesn't load efficiently. see old zshrc for prior art.
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
# zgenom oh-my-zsh plugins/fzf
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
zgenom load chrissicool/zsh-256color

zgenom load hlissner/zsh-autopair

zgenom load zsh-users/zsh-completions src
zgenom load srijanshetty/docker-zsh

# zsh-completion-generator
: {
  GENCOMPL_FPATH="${ZDOTDIR}/completions"
  zgenom load RobSis/zsh-completion-generator
}

# zsh-autosuggestions
: {
  ZSH_AUTOSUGGEST_USE_ASYNC=true
  ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
  ZSH_AUTOSUGGEST_HISTORY_IGNORE="?(#c100,)"
  ZSH_AUTOSUGGEST_MANUAL_REBIND=set
  ZSH_AUTOSUGGEST_STRATEGY=(history completion)
  ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(bracketed-paste)
  ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(autopair-insert)

  zgenom load zsh-users/zsh-autosuggestions
}

# Prompt
zgenom load romkatv/powerlevel10k powerlevel10k

zgenom save
