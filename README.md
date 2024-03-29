This is going to be a completely new configuration, using org-babel to
document and compile the configuration.

TODO: These instructions need to be updated, as well.

This is just a github version of my emacs configuration, for my sanity. :)

Uses [`use-package`][1] and [`straight`][5] to manage package installation and
configuration. Emacs should bootstrap `use-package` and install all
other packages automatically when starting up for the first time.

Install the [OS X version of emacs][2], and make sure that the font
[Fira Code][3] is installed (this can be installed via `homebrew` at
this point).


Getting started with a new checkout:
``` shell
brew cask install font-fira-code
brew tap railwaycat/emacsmacport
brew install --with-modern-icon --with-natural-title-bar --with-tree-sitter --with-native-compilation emacs-mac
```

Because my configuration is now a literate org file, I have checked in
a pre-baked version of `init.el` and `early-init.el`. Copy these files
into `~/.config/emacs`.

In order to guarantee a working configuration, you can use the
straight version snapshot stored in `straight-default.el`.  Copy this
file to `~/.config/emacs/straight/versions/default.el`.

In order to finish this setup, you will need to run the following
command inside of emacs to download and install some additional fonts:

``` shell
M-x all-the-icons-install-fonts
```

Finally, ensure that [Emacs respects the title bar setting][4]:
``` shell
defaults write org.gnu.Emacs TransparentTitleBar DARK
```

[1]: https://github.com/jwiegley/use-package
[2]: https://github.com/railwaycat/homebrew-emacsmacport
[3]: https://github.com/tonsky/FiraCode
[4]: https://github.com/railwaycat/homebrew-emacsmacport/wiki/Natural-Title-Bar
[5]: https://github.com/radian-software/straight.el
