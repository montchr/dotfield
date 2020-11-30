# Non-interactive users (sshy, sync)
if not status --is-interactive
    exit # Skips the rest of this file, not exiting the shell
end

set -xg TERM "xterm-256color"
set -xg DOTS "$HOME/.dots"

source $__fish_config_dir/__abbreviations.fish
source $__fish_config_dir/__aliases.fish
source $__fish_config_dir/__env.fish
source $__fish_config_dir/__path.fish

# Completions
# for completion_dir in $DOTFILES/*/completions
#     set -p fish_complete_path $completion_dir
# end

# Functions
# for func_dir in $DOTFILES/*/functions
#     set -p fish_function_path $func_dir
# end

# rbenv
# status --is-interactive; and source (rbenv init -|psub)

# pyenv
status --is-interactive; and source (pyenv init -|psub)

# starship
starship init fish | source

# direnv
direnv hook fish | source

if test -e $HOME/.localrc
    source $HOME/.localrc
end

# emacs-libvterm integration
function vterm_printf
    if [ -n $TMUX ]
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" $argv
    else if string match -q -- "screen*" $TERM
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" $argv
    else
        printf "\e]%s\e\\" $argv
    end
end

fish_ssh_agent
