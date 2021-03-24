#!/usr/bin/env bash

cd "${HOME}" || return 1

cp /vagrant/bootstrap "${HOME}/bootstrap"
bash -c "${HOME}/bootstrap"
