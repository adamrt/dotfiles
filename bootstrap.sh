#!/bin/sh

# wget -qO- https://raw.githubusercontent.com/adamrt/dotfiles/master/bootstrap.sh

set -e

if [ -d ~/.dotfiles ]; then
	cd ~/.dotfiles
	git pull --ff-only
else
	git clone https://github.com/adamrt/dotfiles ~/.dotfiles
fi

cd ~/.dotfiles
for f in .???*; do
	rm -f ~/$f
	(cd ~/; ln -s .dotfiles/$f $f)
done
