function jira --description "Open this branch's Jira ticket in Chrome"
    git rev-parse --abbrev-ref HEAD | read -l this_branch
    echo $this_branch | egrep -oe "[A-Z]+-[0-9]+" | read -l this_ticket
    open -a "Google Chrome.app" "https://alleyinteractive.atlassian.net/browse/$this_ticket"
end
