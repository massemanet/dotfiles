#!/bin/bash
# -*- mode: shell-script -*-
# ~/.bashrc: executed by bash(1) for non-login shells.

# make scp work by checking for a tty
[ -t 0 ] || return
[[ "${-}" =~ 'i' ]] || return

# clean up
unalias -a

# check terminal resize
shopt -s checkwinsize

# pretty colors
export LS_COLORS="di=36:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:or=30;41:mi=30;46"

# emacs
export EDITOR=nano
>/dev/null command -v emacs &&
    export EDITOR="emacsclient -ct -a ''"

# ssh agent
eval "$(ssh-agent -s)"

# PS1
export GIT_PS1_SHOWSTASHSTATE=true
export GIT_PS1_SHOWUNTRACKEDFILES=true
export GIT_PS1_SHOWDIRTYSTATE=true
export GIT_PS1
export LAST_EXIT
export SSHID
export PROMPT_COMMAND='_prompt LAST_EXIT; _prompt history; _prompt GIT_PS1; _prompt SSHID'

PS1='\[\e[33m\]\h'
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

e() {
    if ! emacsclient -ct "$1"
    then if emacs --daemon
	 then emacsclient -ct "$1"
	 else "failed to start emacs server"
	 fi
    fi
}

_gitps1() {
    if ! GDIR="$(2>/dev/null git rev-parse --show-toplevel)"
    then echo ":"
    else GBRANCH="$(__git_ps1 "%s")"
         if [ "$GDIR" == "$HOME" ]
         then echo "~:$GBRANCH"
         else echo "$(basename "$GDIR"):$GBRANCH"
         fi
    fi
}

_prompt() {
    case "${1:-}" in
        LAST_EXIT) LAST_EXIT=$?; [ $LAST_EXIT == 0 ] && unset LAST_EXIT;;
        GIT_PS1)   GIT_PS1="$(_gitps1)";;
        SSHID)     SSHID="$(~/bin/sshid.sh)" || unset SSHID;;
        history)   history -a;;
    esac
}

## history
# unlimited history
export HISTSIZE=
export HISTFILESIZE=

# agglomerate history from multiple shells
export HISTCONTROL="ignoredups"
shopt -s histappend

# multi-line commands
shopt -s cmdhist

# motd
uname -a
