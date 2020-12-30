#!/usr/bin/env zsh
#
# Toggle system dark mode and run various related effects.
#

dark-mode

[[ $(dark-mode status) = 'on' ]] &&
	{
		kitty @ --to unix:/tmp/kitty-socket \
			set-colors -a -c "~/.config/kitty/dark-theme.conf"
		emacsclient -e "(load-theme 'doom-monokai-pro)"
	} ||
	{
		kitty @ --to unix:/tmp/kitty-socket \
			set-colors -a -c "~/.config/kitty/light-theme.conf"
		emacsclient -e "(load-theme 'doom-plain)"
	}
