#!/usr/bin/env bash

command -v emacs     || ~/bin/install.sh emacs
command -v erl       || ~/bin/install.sh erlang
command -v rebar3    || ~/bin/install.sh rebar
command -v bazel     || ~/bin/install.sh bazelisk
command -v kubectl   || ~/bin/install.sh kubectl
command -v aws       || ~/bin/install.sh awscli
command -v aws-vault || ~/bin/install.sh aws-vault
command -v gopass    || ~/bin/install.sh gopass
command -v pass      || ~/bin/install.sh pass
command -v docker    || ~/bin/install.sh docker

export AWS_VAULT_BACKEND=pass
export AWS_VAULT_PASS_PREFIX=aws-vault
export GPG_TTY
GPG_TTY="$(tty)"
