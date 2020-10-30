#!/usr/bin/env zsh

if
	! command -v luarocks &
	>/dev/null
then
	brew install luarocks
fi

if
	! command -v fennel &
	>/dev/null
then
	luarocks install fennel
fi
