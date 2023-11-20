# shellcheck source=.profile
. ~/.profile
. ~/.bashrc
# shellcheck disable=SC1091
if [ -f "$HOME/.wrk.sh" ]
then source "$HOME/.wrk.sh"
fi
