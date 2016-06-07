This is just a github version of my emacs configuration, for my sanity. :)

This now uses Cask and Pallet to manage packages, and should be used
with the [OS X version of emacs][1]. Also, make sure that the font
[Anonymous Pro][2] is installed.

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

[1]: https://github.com/railwaycat/homebrew-emacsmacport
[2]: http://www.marksimonson.com/fonts/view/anonymous-pro
