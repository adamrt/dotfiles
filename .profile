# $OpenBSD: dot.profile,v 1.5 2018/02/02 02:29:54 yasuoka Exp $
#
# sh/ksh initialization

# Only for OpenBSD. Reverse the logic next time on OpenBSD
if [[ `uname` != 'Linux ']]; then
    export ONLY_OPENBSD=1
    PATH=$HOME/bin:$HOME/go/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games
    export PATH HOME TERM

    # Set non-login shells to evaluate .kshrc
    ENV=$HOME/.kshrc
    export ENV
fi
