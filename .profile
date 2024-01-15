#!/usr/bin/env bash

# one path to rule them all
export PATH=/home/masse/bin:/usr/local/bin:/usr/bin:/usr/sbin

# shellcheck disable=SC1091
[ -f "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"

# keep the locale simple
unset LC_CTYPE
unset LC_ALL
unset LANGUAGE
LANG="$(locale -a | grep -Ei "en.us.utf" || locale -a | grep -Ei "c.utf" || echo "C")"
