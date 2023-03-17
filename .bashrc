#!/bin/bash
# -*- mode: shell-script -*-
# ~/.bashrc: executed by bash(1) for non-login shells.

# make scp work by checking for a tty
[ -t 0 ] || return
[[ "${-}" =~ 'i' ]] || return

# ssh-agent
[ -z "${SSH_AUTH_SOCK:-}" ] && exec ssh-agent bash -l

# clean up
unalias -a

# check terminal resize
shopt -s checkwinsize

# history: unlimited, no dupes, no ts, handle multiple shells
shopt -s histappend
shopt -s cmdhist
export HISTSIZE=
export HISTFILESIZE=
export HISTCONTROL="ignoredups"
unset HISTTIMEFORMAT

# pretty colors
export LS_COLORS="di=36:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:or=30;41:mi=30;46"

# PS1
export GIT_PS1_SHOWSTASHSTATE=true
export GIT_PS1_SHOWUNTRACKEDFILES=true
export GIT_PS1_SHOWDIRTYSTATE=true
export GIT_PS1
export LAST_EXIT
export SSHID
export PROMPT_COMMAND='_prompt LAST_EXIT; _prompt history; _prompt GIT_PS1; _prompt SSHID; _prompt K8SCTX'

PS1='\[\e[33m\]\h'
PS1+='\[\e[32m\]${K8SCTX:+<${K8SCTX}>}'
PS1+='\[\e[36m\]${SSHID:+[${SSHID}]}'
PS1+='\[\e[35m\]($GIT_PS1)'
PS1+='\[\e[32m\]${LAST_EXIT:+\[\e[31m\]($LAST_EXIT)}$'
PS1+='\[\e[0m\] '

# aliases
dir()  { ls -AlFh --color "$@"; }
dirt() { dir -rt "$@"; }
dird() { dir -d "$@"; }
rea()  { history | grep -E "${@:-}"; }
c()    { cat "$@"; }
m()    { less "$@"; }

# emacs
if >/dev/null command -v emacs
then export EDITOR="emacsclient -nw -c --alternate-editor="
else export EDITOR=nano
fi

e() {
    case "${1:-}" in
        "") $EDITOR ;;
        *)  $EDITOR -c "$1";;
    esac
}

_gitps1() {
    local DIR BRANCH
    if DIR="$(2>/dev/null git rev-parse --show-toplevel)"
    then BRANCH="$(2>/dev/null __git_ps1 "%s")"
         if [ "$DIR" == "$HOME" ]
         then echo "~:$BRANCH"
         else echo "$(basename "$DIR"):$BRANCH"
         fi
    else echo ":"
    fi
}

_prompt() {
    case "${1:-}" in
        LAST_EXIT) LAST_EXIT=$?; [ $LAST_EXIT == 0 ] && unset LAST_EXIT;;
        GIT_PS1)   GIT_PS1="$(_gitps1)";;
        SSHID)     SSHID="$(~/bin/sshid.sh)" || unset SSHID;;
        K8SCTX)    K8SCTX="$(~/bin/k8s)" || unset K8SCTX;;
        history)   history -a;;
    esac
}
