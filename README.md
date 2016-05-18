This is just a github version of my emacs configuration, for my sanity. :)

This now uses Cask and Pallet to manage packages, and should be used
with the version of emacs provided via
https://github.com/railwaycat/homebrew-emacsmacport.

Getting started with a new checkout:
```
# install emacs first
brew tap railwaycat/emacsmacport
brew install --with-modern-icon emacs-mac

# now install required packages
brew install cask
cd ~/.emacs.d
cask install
```
