This is just a github version of my emacs configuration, for my sanity. :)

Uses [`use-package`][1] to manage package installation and
configuration. Emacs should bootstrap `use-package` and install all
other packages automatically when starting up for the first time.

Install the [OS X version of emacs][2], and make sure that the font
[Fira Code][3] is installed (this can be installed via `homebrew` at
this point).


Getting started with a new checkout:
``` shell
brew cask install font-fira-code
brew tap railwaycat/emacsmacport
brew install --with-modern-icon emacs-mac
```

In order to finish this setup, you will need to run the following
command inside of emacs to download and install some additional fonts:

``` shell
M-x all-the-icons-install-fonts
```

[1]: https://github.com/jwiegley/use-package
[2]: https://github.com/railwaycat/homebrew-emacsmacport
[3]: https://github.com/tonsky/FiraCode
