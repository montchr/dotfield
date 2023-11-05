#!/usr/bin/env zsh

### :: KEYBINDINGS ::

##: Reference
#
# - https://wiki.archlinux.org/title/zsh#Key_bindings
# - https://sgeb.io/posts/zsh-zle-custom-widgets/
# - https://man.archlinux.org/man/user_caps.5

# typeset -gA Keys

# zmodload zsh/terminfo

# FIXME: breaks backspace for example in wezterm
# Keys[Home]="${terminfo[khome]}"
# Keys[End]="${terminfo[kend]}"
# Keys[Insert]="${terminfo[kich1]}"
# Keys[Backspace]="${terminfo[kbs]}"
# Keys[Delete]="${terminfo[kdch1]}"
# Keys[Up]="${terminfo[kcuu1]}"
# Keys[Down]="${terminfo[kcud1]}"
# Keys[Left]="${terminfo[kcub1]}"
# Keys[Right]="${terminfo[kcuf1]}"
# Keys[PageUp]="${terminfo[kpp]}"
# Keys[PageDown]="${terminfo[knp]}"
# Keys[Shift-Tab]="${terminfo[kcbt]}"


##: --- insert mode ---

bindkey -M viins '^A' beginning-of-line
bindkey -M viins '^E' end-of-line
bindkey -M viins '^D' push-line-or-edit


##: --- plugins ---

# <zsh-users/zsh-history-substring-search>
bindkey "^P" history-substring-search-up
bindkey "^N" history-substring-search-down
