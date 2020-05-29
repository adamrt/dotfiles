# GPG config
export GPG_TTY=$(tty)
gpgconf --launch gpg-agent


# Directory tracking for ansi-term. This might make more sense in
# ~/.profile
[[ -n $INSIDE_EMACS ]] && cd() {
 builtin cd "$@" && { echo '\032/\c'; pwd; } >/dev/tty
}
