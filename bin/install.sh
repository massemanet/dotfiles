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

_elisp() {
    [ -z "${1:-}" ] && _err "Provide elisp."
    2>&1 emacs --no-site-file --batch --eval "$1"
}

_apt_install() {
    sudo apt-get update &&
        sudo apt-get install -yq --no-install-recommends "$@"
}

_github () {
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

#######################################################

get-awscli() {
    ## in EC2, the completer is built in. see "$(command -v aws_completer)"
    _apt_install awscli
    echo "awscli."
}

get-aws-vault() {
    _github "99designs" "aws-vault" "-linux-amd64" "${1:-}"
    curl -fsSLo https://raw.githubusercontent.com/99designs/aws-vault/v6.6.2/contrib/completions/bash/aws-vault.bash /tmp/aws-vault-complete &&
        sudo cp /tmp/aws-vault-complete /etc/bash_completion.d/
}

get-bazelisk() {
    local COMPLETER=~/git/loltel/script/bazel-complete.bash
    _github "bazelbuild" "bazelisk" "-linux-amd64" "${1:-}"
    [ -f "$COMPLETER" ] && sudo ln -s "$COMPLETER" /etc/bash_completion.d/
    rm -f ~/bin/bazel && ln -s /usr/local/bin/bazelisk ~/bin/bazel
    cat > ~/.bazelrc <<<"build --disk_cache=/tmp/bazel"
}

get-docker() {
    local AUSER="$USER"
    _apt_install docker.io docker-compose &&
        sudo adduser "$AUSER" docker
    echo "installed docker"
}

get-docker-cred() {
    local GH="https://github.com/docker/docker-credential-helpers/releases"
    local RE="download/v[0-9\\.]+/docker-credential-pass-v[0-9\\.]+-amd64.tar.gz"
    local GH=https://github.com/docker/docker-credential-helpers/releases/download/v0.7.0/docker-credential-pass-v0.7.0.linux-amd64
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

# init emacs
get-emacs() {
    command -v emacs || _apt_install emacs-nox aspell-en
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

# install erlang
get-erlang() {
    local VSN="${1:-26}"

    _apt_install \
        build-essential \
        ca-certificates \
        libncurses-dev \
        libpcap-dev \
        libsctp-dev \
        libsctp1 \
        libssl-dev \
        lksctp-tools \
        make
    sudo chmod a+w /opt
    [ -d ~/git/otp ] || git clone --depth=1 https://github.com/erlang/otp.git ~/git/otp
    cd ~/git/otp/
    git remote set-branches origin 'maint-*'
    git fetch -v
    git checkout "origin/maint-$VSN"
    git clean -fdx
    ./configure \
        --prefix="/opt/erl$VSN" \
        --without-debugger \
        --without-eldap \
        --without-erl_docgen \
        --without-et \
        --without-ftp \
        --without-hipe \
        --without-javac \
        --without-jinterface \
        --without-megaco \
        --without-observer \
        --without-odbc \
        --without-tftp \
        --without-wx \
        --without-dynamic-trace \
        --enable-sctp=lib \
        --disable-lock-counter
    make -j8
    make -j8 install
}

get-et() {
    if grep -q Ubuntu /etc/lsb-release
    then sudo add-apt-repository ppa:jgmath2000/et
    else URL="https://mistertea.github.io/debian-et"
         echo "deb $URL/debian-source/ bullseye main" | sudo tee /etc/apt/sources.list.d/et.list
         curl -sS "$URL/et.gpg" | sudo apt-key add -
    fi
    _apt_install et
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

get-gopass() {
    _apt_install gopass
}

get-kubectl() {

    local KEYRING=/etc/apt/keyrings/kubernetes-archive-keyring.gpg
    local APTKEY=https://dl.k8s.io/apt/doc/apt-key.gpg
    local REPO=https://apt.kubernetes.io/
    local LIST=/etc/apt/sources.list.d/kubernetes.list
    _apt_install ca-certificates curl &&
        sudo mkdir -p /etc/apt/keyrings &&
        sudo curl -fsSLo "$KEYRING" "$APTKEY" &&
        echo "deb [signed-by=$KEYRING] $REPO kubernetes-xenial main" | sudo tee "$LIST" &&
        _apt_install kubectl &&
        kubectl completion bash | sudo tee /etc/bash_completion.d/kubectl-complete > /dev/null
}

get-pass() {
    _apt_install pass
}

get-pre-commit() {
    _apt_install python3-pip
    pip install pre-commit
}

get-rebar() {
    local VSN="${1:-26}"
    local ERLDIR=/opt/erl$VSN/bin

    PATH="$ERLDIR:$PATH"
    if [[ "$(command -v erl)" == "$ERLDIR/erl" ]]
    then echo "Found erlang $VSN"
    else get-erlang "${VSN}"
    fi
    [ -d ~/git/rebar3 ] || git -C ~/git clone http://github.com/erlang/rebar3
    cd ~/git/rebar3
    git pull
    ./bootstrap
    cp rebar3 "$(dirname "$(command -v erl)")"
}

get-rust() {
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /tmp/rust.sh
    chmod +x /tmp/rust.sh
    /tmp/rust.sh --no-modify-path -y -q
}

get-skopeo() {
    local GOPATH="${GOPATH:-/tmp}"
    local URL="github.com/containers/skopeo"
    local SKOP="$GOPATH"/src/"$URL"

    git clone https://"$URL" "$SKOP"
    cd "$SKOP"
    make bin/skopeo
    mkdir -p "$HOME"/.config/containers
    cp "$SKOP"/default-policy.json "$HOME"/.config/containers/policy.json
    cp bin/skopeo ~/bin/
}

get-tshark() {
    _apt_install tshark
    sudo adduser "$USER" wireshark
}

[ -z "${1:-}" ] && _usage
sudo true
TRG="$1"
VSN="${2:-}"
echo "## $TRG:$VSN ##################################################################"
"get-$TRG" "$VSN"
