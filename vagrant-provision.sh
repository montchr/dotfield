#!/usr/bin/env bash

sudo su -

echo "$GIT_BRANCH"
bootstrap_url="https://raw.github.com/montchr/dots/${GIT_BRANCH}/bootstrap"

bash -c "$(wget -qO - "${bootstrap_url}")"
