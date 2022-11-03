#!/usr/bin/env bash

set -euo pipefail

usage() {
    echo "$0 TARGET [VSN]"
    err ""
}

err() {
    echo "$1"
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

# emacs for wayland
get-emacs() {
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
    local VSN="${1:-23}"

    command -v make > /dev/null || err "install 'make'"
    command -v automake > /dev/null || err "install 'automake'"
    command -v autoconf > /dev/null || err "install 'autoconf'"

    [ -d ~/git/otp ] || git clone --depth=1 https://github.com/erlang/otp.git ~/git/otp
    cd ~/git/otp/
    git remote set-branches origin 'maint-*'
    git fetch -v
    git co "maint-$VSN"
    git pull --depth=2 --ff-only
    git clean -fdx
    ./otp_build autoconf
    ./configure --without-megaco --without-odbc --without-jinterface --without-javac
    make
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

get-rebar() {
    local VSN="${1:-}"

    local DLPAGE="https://github.com/erlang/rebar3/releases"
    local RE="download/[0-9\\.]+/rebar3"
    r="$(curl -sL "$DLPAGE" | grep -oE "$RE" | grep "$VSN" | sort -uV | tail -n1)"
    [ -z "$r" ] && err "no file at $DLPAGE."
    echo "found file: $r"
    curl -sL "$DLPAGE/$r" -o /tmp/rebar3
    sudo install /tmp/rebar3 /usr/local/bin
}

get-rust() {
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /tmp/rust.sh
    chmod +x /tmp/rust.sh
    /tmp/rust.sh --no-modify-path -y -q
}

sudo true
[ -z "$1" ] && usage
TRG="$1"
VSN="${2:-}"
echo "## $TRG:$VSN ##################################################################"
"get-$TRG" "$VSN"
