#!/usr/bin/env bash

set -euo pipefail
shopt -s nullglob

_err() {
    echo "${1:-}"
    exit 12
}

_usage() {
    cat <<HERE
$0 [show | rm | all | list | add <ID>]

$0 works with IDs (a.k.a. "comments"); it doesn't care about filenames.
It will look for identities in ~/git/pet/files/ssh and ~/.ssh.

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

_file2id() {
    cut -f3 -d" " < "$1"
}

_id2file() {
    local ID="$1"

    for file in $(_files)
    do grep -q "$ID" "$file" && echo "${file%.pub}" || true
    done
}

_show() {
    local X=""
    if is="$(ssh-add -l | cut -f3 -d" ")"
    then for i in $is
	 do if [ -z "$X" ]
	    then X="$i"
	    else X+=":$i"
	    fi
	 done
    fi
    echo "$X"
}

_list() {
    for f in $(_files)
    do _file2id "$f"
    done    
}

_add() {
    local ID="$1"

    if file="$(_id2file "$ID")" && [ -f "$file" ]
    then ssh-add "$file"
    else _err "no such id: $ID"
    fi
}

_all() {
    for i in $(_list)
    do _add $i
    done
}

_rm() {
    ssh-add -D
}

_rename() {
    local OLDID="$1"
    local NEWID="$2"
    local file

    [ -z "$OLDID" -o -z "$NEWID" ] && _err "usage: rename <OLDID> <NEWID>"
    file="$(_id2file "$OLDID")"
    [ ! -f "$file" ] && _err "no such identity: $OLDID"
    ssh-keygen -c -C "$NEWID" -f "$file"
}

case "${1:-}" in
    ""|show)   _show;;
    rm)     _rm;;
    all)    _all;;
    list)   _list;;
    add)    _add "${2:-}";;
    rename) _rename "${2:-}" "${3:-}";;
    *)      _err "no such command: $1"
esac
