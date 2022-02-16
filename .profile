#!/usr/bin/env bash

# one path to rule them all
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin

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

# one locale to rule them all
unset  LC_ALL
unset  LANGUAGE
L="$(locale -a | grep -Ei "en.us.utf")"
if [ -z "$L" ]
then export LANG="C"
else export LANG="$L"
fi
