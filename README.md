This is going to be a completely new configuration, using org-babel to
document and compile the configuration.

This is just a github version of my emacs configuration, for my sanity. :)

Uses [`use-package`][1] and [`straight`][4] to manage package installation and
configuration. Emacs should bootstrap `use-package` and install all
other packages automatically when starting up for the first time.

Install the [emacs-mac][2], and make sure that the font
[Jetbrains Mono][3] is installed (this can be installed via `homebrew` at
this point).

Getting started with a new checkout:
``` shell
brew install font-jetbrains-mono
brew tap railwaycat/emacsmacport
brew install railwaycat/emacsmacport/emacs-mac --with-natural-title-bar --with-native-compilation --with-sjrmanning-icon --with-tree-sitter
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
M-x nerd-icons-install-fonts
```

[1]: https://github.com/jwiegley/use-package
[2]: https://github.com/railwaycat/homebrew-emacsmacport
[3]: https://www.jetbrains.com/lp/mono/
[4]: https://github.com/radian-software/straight.el
