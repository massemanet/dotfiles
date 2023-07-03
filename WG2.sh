#!/usr/bin/env bash

export AWS_VAULT_BACKEND=pass
export AWS_VAULT_PASS_PREFIX=aws-vault
export GPG_TTY
GPG_TTY="$(tty)"

>/dev/null command -v bazel     || ~/bin/install.sh bazelisk
>/dev/null command -v kubectl   || ~/bin/install.sh kubectl
>/dev/null command -v aws       || ~/bin/install.sh awscli
>/dev/null command -v aws-vault || ~/bin/install.sh aws-vault
>/dev/null command -v pass      || ~/bin/install.sh pass
>/dev/null command -v docker    || ~/bin/install.sh docker
