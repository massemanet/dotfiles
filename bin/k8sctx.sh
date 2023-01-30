#!/usr/bin/env bash

_context() {
    local CLUSTER CONTEXT
    CLUSTER="$(kubectl config get-clusters | grep "${1:-}")" &&
        [ "$(wc -l <<<"$CLUSTER")" == 1 ] &&
        CONTEXT="$(kubectl config get-users | grep "$CLUSTER")" &&
        [ "$(wc -l <<<"$CONTEXT")" == 1 ] &&
        echo "$CONTEXT"
}

_show() {
    kubectl config current-context | grep -Eo '\-[a-z]*\.[a-z]*' | tr -d "-"
}

_set() {
    local CONTEXT
    CONTEXT="$(_context "$1")"
    if [ -z "$CONTEXT" ]
    then echo "no context matching $1"
    else kubectl config use-context "$CONTEXT"
    fi
}

case "${1:-}" in
    "") _show;;
    *)  _set "$1";;
esac
