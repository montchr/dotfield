#!/usr/bin/env zsh

### :: PLUGINS ::

if ! zgenom saved; then
  echo "Initialising zgenom"

  # Purge compiled and cached files
  rm -f $ZDOTDIR/*.zwc(N) \
        $ZSH_CACHE/*(N) \
        $ZGEN_DIR/init.zwc

  zgenom compdef

  # NOTE: Be extra careful about plugin load order, or subtle breakage can emerge.
  #       fzf-tab, for example, must be loaded before any widgets.
  zgenom load Aloxaf/fzf-tab
  zgenom load zsh-users/zsh-syntax-highlighting
  zgenom load zsh-users/zsh-completions src
  zgenom load zsh-users/zsh-autosuggestions
  zgenom load zsh-users/zsh-history-substring-search
  zgenom load romkatv/powerlevel10k powerlevel10k
  zgenom load hlissner/zsh-autopair autopair.zsh

  zgenom save
  zgenom compile $ZDOTDIR
fi
