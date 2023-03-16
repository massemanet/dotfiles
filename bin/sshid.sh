#!/usr/bin/env bash

set -euo pipefail
shopt -s nullglob

_err() {
    echo "${1:-}"
    exit 12
}

_usage() {
    local PROG
    PROG=$(basename "$0")
    cat <<HERE
$PROG [help | show | rm | all | list | add <ID>]

$PROG works with IDs (a.k.a. "comments"); it doesn't care about filenames.
It will look for identities in ~/git/pet/files/ssh and ~/.ssh.

  help
    show this text
  show
    show the currently active id(s)
  rm
    remove all ids from the agent
  all
    add all ids to the agent
  add <ID>
    add <ID> to the agent
  list
    list all available ids
  rename <OLDID> <NEWID>
    give the <ID> of an identity.

HERE
}

_files() {
    echo ~/git/pet/files/ssh/*pub ~/.ssh/*pub
}

_to_id() {
    local F="${1%.pub}.pub"

    if [ -f "$F" ]
    then cut -f3 -d" " < "$F"
    else echo "$1"
    fi
}

_to_file() {
    if [ -f "$1" ]
    then echo "$1"
    else for file in $(_files)
         do if [[ $(_to_id "$file") == "$1" ]]
            then echo "${file%.pub}"
            fi
         done
    fi
}

_show() {
    local ID IDs=""

    if is="$(2>/dev/null ssh-add -l | cut -f3 -d" ")"
    then for i in $is
         do ID="$(_to_id "$i")"
            if [ -z "$IDs" ]
            then IDs="$ID"
            else IDs+=":$ID"
            fi
         done
    fi
    echo "$IDs"
}

_list() {
    for f in $(_files)
    do _to_id "$f"
    done
}

_add() {
    local ID="$1"

    if file="$(_to_file "$ID")" && [ -f "$file" ]
    then ssh-add "$file"
    else _err "no such id: $ID"
    fi
}

_all() {
    for i in $(_list)
    do _add "$i"
    done
}

_rm() {
    ssh-add -D
}

_rename() {
    local OLDID="$1"
    local NEWID="$2"
    local file

    [ -z "$OLDID" ] || [ -z "$NEWID" ] && _err "usage: rename <OLDID> <NEWID>"
    file="$(_to_file "$OLDID")"
    [ ! -f "$file" ] && _err "no such identity: $OLDID"
    ssh-keygen -c -C "$NEWID" -f "$file"
}

case "${1:-}" in
    ""|show) _show;;
    help)    _usage;;
    rm)      _rm;;
    all)     _all;;
    list)    _list;;
    add)     _add "${2:-}";;
    rename)  _rename "${2:-}" "${3:-}";;
    *)       _err "no such command: $1"
esac
