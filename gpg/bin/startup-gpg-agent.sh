# Ensure that gpg can find the agent when needed
if [ -f ~/.gnupg/.gpg-agent-info ] && [ -n "$(pgrep gpg-agent)" ]; then
    source ~/.gnupg/.gpg-agent-info
    export GPG_AGENT_INFO
else
    eval $(gpg-agent --daemon --write-env-file ~/.gnupg/.gpg-agent-info)
fi

# This line is important for GUI tools to also find it
launchctl setenv GPG_AGENT_INFO $GPG_AGENT_INFO
