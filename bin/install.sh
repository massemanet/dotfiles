#!/usr/bin/env bash

set -euo pipefail

_usage() {
    echo "$(basename "$0") TARGET [VSN]"
    exit 0
}

_err() {
    echo "${1:-}"
    exit 1
}

get-docker() {
    local r s

    B="https://download.docker.com/linux/debian/dists/buster/pool/test/amd64"
    r="$(curl -sSL "$B")"
    for s in containerd.io docker-ce-cli docker-ce
    do RE="${s}_[^_]*_amd64.deb"
       S="$(echo "$r" | grep -Eo "$RE" | sort -urV | head -n 1)"
       echo "$S"
       curl -sSL "$B/$S" > /tmp/$$
       sudo dpkg -i /tmp/$$
    done

    groups | grep docker || sudo adduser "$USER" docker

    local GH="https://github.com/docker/docker-credential-helpers/releases"
    local RE="download/v[0-9\\.]+/docker-credential-pass-v[0-9\\.]+-amd64.tar.gz"
    r="$(curl -sSL "$GH" | grep -Eo "$RE" | grep "$VSN" | sort -Vu | tail -n1)"
    echo "found file $r"
    curl -sSL "$GH/$r" > /tmp/docker_cred_helper.tgz
    rm -rf ~/pet/docker
    mkdir -p ~/pet/docker
    mkdir -p ~/.docker
    tar -C ~/pet/docker -xzf /tmp/docker_cred_helper.tgz
    chmod +x ~/pet/docker/docker-credential-pass
    echo '{"credsStore": "pass"}' > ~/pet/docker/config.json
    (cd ~/bin; ln -s ../pet/docker/docker-credential-pass . ; cd ~/.docker ; ln -s ../pet/docker/config.json .)
}


_elisp() {
    [ -z "${1:-}" ] && _err "Provide elisp."
    2>&1 emacs --no-site-file --batch --eval "$1"
}

# init emacs
get-emacs() {
    command -v emacs || sudo apt-get install -y emacs-nox
    EMACSDIR="$(eval readlink -f "$(_elisp "(message user-emacs-directory)")")" &&
        rm -rf ~/.emacs.d &&
        ln -s ~/.config/emacs ~/.emacs.d &&
        [ -f "$EMACSDIR/init.el" ] &&
        _elisp "(require 'init \"$EMACSDIR/init.el\")" &&
        [ -d "$EMACSDIR/straight/repos/otp" ] &&
        [ ! -L "$EMACSDIR/straight/repos/otp" ] &&
        mkdir -p ~/git &&
        rm -rf ~/git/otp &&
        mv "$EMACSDIR/straight/repos/otp" ~/git &&
        ln -s ~/git/otp "$EMACSDIR/straight/repos" &&
        echo "installed emacs"
}

# emacs for wayland
get-emacs-wayland() {
    cd ~/git
    [ -d emacs ] || git clone --branch feature/pgtk --single-branch git://git.sv.gnu.org/emacs.git
    cd emacs/
    git sync
    ./autogen.sh
    ./configure --with-pgtk --with-json --with-native-compilation --with-file-notification=inotify
    sudo make install
}

# install erlang + rebar + redbug
get-erlang() {
    local VSN="${1:-25}"

    command -v make > /dev/null || _err "install 'make'"
    command -v automake > /dev/null || _err "install 'automake'"
    command -v autoconf > /dev/null || _err "install 'autoconf'"

    sudo apt-get update &&
        sudo apt-get install -yq --no-install-recommends \
             autoconf \
             build-essential \
             ca-certificates \
             liblttng-ust-dev \
             liblttng-ust0 \
             libncurses-dev \
             libpcap-dev \
             libpcap0.8 \
             libsctp-dev \
             libsctp1 \
             libssl-dev \
             libssl1.1 \
             lksctp-tools \
             make
    [ -d ~/git/otp ] || git clone --depth=1 https://github.com/erlang/otp.git ~/git/otp
    cd ~/git/otp/
    git remote set-branches origin 'maint-*'
    git fetch -v
    git co "maint-$VSN"
    git pull --depth=1 --ff-only
    git clean -fdx
    ./configure \
        --without-javac --without-jinterface --without-odbc --without-megaco \
        --with-dynamic-trace=lttng --enable-hipe --enable-sctp=lib \
        --enable-lock-counter
    make -j4
    sudo make install
}

get_et() {
    URL="https://mistertea.github.io/debian-et"
    echo "$URL/debian-source/ bullseye main" | sudo tee /etc/apt/sources.list.d/et.list
    curl -sS "$URL//et.gpg" | sudo apt-key add -
    sudo apt update && \
        sudo apt install -y et
}

get-go() {
    local DL="golang.org/dl"
    local RE="go[0-9]+\.[0-9]+\.[0-9]+\.linux-amd64\.tar\.gz"
    local TGZ

    TGZ="$(curl -sSL "$DL" | grep -Eo "$RE" | sort -rV | head -n1)"
    echo "found $TGZ"
    curl -sSL "$DL/$TGZ" > /tmp/$$.tgz
    sudo tar -C /usr/local -xzf /tmp/$$.tgz
}

_get-github () {
    local ORG="$1"
    local PROJ="$2"
    local BIN="$3"
    local VSN="${4:-v?[0-9\.]+}"
    local TARG="${5:-/usr/local/bin}"

    local RE="$ORG/$PROJ/releases/tag/$VSN"
    local DLPAGE="https://github.com/$ORG/$PROJ"
    VSN="$(curl -sL "$DLPAGE"/tags | grep -Eo "$RE" | grep -Eo "$VSN" | sort -Vru | head -n1)"
    [ -z "$VSN" ] && _err "no file at $DLPAGE."
    echo "found file: $VSN"
    curl -sL "$DLPAGE/releases/download/$VSN/$PROJ$BIN" -o "/tmp/$PROJ"
    sudo install "/tmp/$PROJ" "$TARG"
    echo "installed $PROJ::$VSN in $TARG"
}

get-awscli() {
    sudo apt-get install -y awscli &&
        echo "awscli."
##        sudo cp "$(command -v aws_completer)" /etc/bash_completion.d/
}

get-aws-vault() {
    _get-github "99designs" "aws-vault" "-linux-amd64" "${1:-}"
    curl -fsSLo https://raw.githubusercontent.com/99designs/aws-vault/v6.6.2/contrib/completions/bash/aws-vault.bash /tmp/aws-vault-complete &&
        sudo cp /tmp/aws-vault-complete /etc/bash_completion.d/
}

get-bazelisk() {
    local COMPLETER=~/git/loltel/script/bazel-complete.bash
    _get-github "bazelbuild" "bazelisk" "-linux-amd64" "${1:-}"
    [ -f "$COMPLETER" ] && sudo ln -s "$COMPLETER" /etc/bash_completion.d/
    rm -f ~/bin/bazel && ln -s /usr/local/bin/bazelisk ~/bin/bazel
}

get-kubectl() {
    local KEYRING=/etc/apt/keyrings/kubernetes-archive-keyring.gpg
    local APTKEY=https://packages.cloud.google.com/apt/doc/apt-key.gpg
    local REPO=https://apt.kubernetes.io/
    local LIST=/etc/apt/sources.list.d/kubernetes.list
    sudo apt-get install -y ca-certificates curl &&
        sudo mkdir -p /etc/apt/keyrings &&
        sudo curl -fsSLo "$KEYRING" "$APTKEY" &&
        echo "deb [signed-by=$KEYRING] $REPO kubernetes-xenial main" | sudo tee "$LIST" &&
        sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys B53DC80D13EDEF05 &&
        sudo apt-get update &&
        sudo apt-get install -y kubectl &&
        kubectl completion bash | sudo tee /etc/bash_completion.d/kubectl-complete > /dev/null
}

get-rebar() {
    _get-github "erlang" "rebar3" "" "${1:-}"
}

get-rust() {
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /tmp/rust.sh
    chmod +x /tmp/rust.sh
    /tmp/rust.sh --no-modify-path -y -q
}

[ -z "${1:-}" ] && _usage
sudo true
TRG="$1"
VSN="${2:-}"
echo "## $TRG:$VSN ##################################################################"
"get-$TRG" "$VSN"
