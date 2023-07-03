#!/usr/bin/env bash

# one path to rule them all
export PATH=/usr/local/bin:/usr/bin:/usr/sbin

_pds() {
   cat <<HERE
/usr/local/opt/*/libexec/gnubin
/usr/local/opt/*/bin
/usr/local/*/bin
/opt/*/bin
$HOME/.cargo/bin
$HOME/bin
HERE
}
for P in $(_pds)
do [ -d "$P" ] && PATH="$P:$PATH"
done

PATH="$(erlpath 25)"

# keep the locale simple
unset LC_CTYPE
unset LC_ALL
unset LANGUAGE
LANG="$(locale -a | grep -Ei "en.us.utf" || locale -a | grep -Ei "c.utf" || echo "C")"
