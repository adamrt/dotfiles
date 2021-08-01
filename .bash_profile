# ~/.bash_profile: executed by the command interpreter for login shells.

# if running bash
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

export NODEJS_HOME=/usr/local/lib/node/nodejs
export GO_HOME="$HOME/go"
export CARGO_HOME="$HOME/.cargo"

export PATH=$HOME/.local/bin:$GO_HOME/bin:$NODEJS_HOME/bin:$CARGO_HOME/bin:$PATH


# Setup ssh keys at login
if [ ! -S ~/.ssh/ssh_auth_sock ]; then
  eval `ssh-agent`
  ln -sf "$SSH_AUTH_SOCK" ~/.ssh/ssh_auth_sock
fi
export SSH_AUTH_SOCK=~/.ssh/ssh_auth_sock
ssh-add -l > /dev/null || ssh-add
