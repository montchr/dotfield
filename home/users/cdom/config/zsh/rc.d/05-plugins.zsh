#!/usr/bin/env zsh

### :: PLUGINS ::

if ! zgenom saved; then
  echo "Initialising zgenom"
  # TODO: originally also had `$ZGEN_INIT.zwc` but this seems dubious
  rm -f $ZDOTDIR/*.zwc(N) \
        $XDG_CACHE_HOME/zsh/*(N)

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

###: Configuration

##: Defer to atuin/C-r/fzf/history-substring-search for history completion
ZSH_AUTOSUGGEST_STRATEGY=(completion)
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=30
