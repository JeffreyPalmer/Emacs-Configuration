This is just a github version of my emacs configuration, for my sanity. :)

This org-based configuration uses [`use-package`][1] and
[`straight`][4] to manage package installation and
configuration. Emacs should bootstrap `use-package` and install all
other packages automatically when starting up for the first time.

Install the [emacs-mac][2] (actually [this-one][5] until the source
repo gets updated), and make sure that the fonts [Jetbrains Mono][3]
and [Lato][6] are installed (they can be installed via `homebrew` at
this point).

Getting started with a new checkout:
``` shell
brew install font-jetbrains-mono
brew install font-lato
git clone https://github.com/jdtsmith/emacs-mac.git
```

Then follow the installation instructions in the README.txt file to
build and install emacs.

Because this configuration is a literate org file, I have checked in a
pre-baked version of `init.el` and `early-init.el`. Copy these files
into `~/.config/emacs` before starting up emacs for the first time.

In order to guarantee a known-good working configuration, you can use
the straight version snapshot stored in `straight-default.el`.  Copy
this file to `~/.config/emacs/straight/versions/default.el` and then
run 'M-x straight-thaw-versions' from within emacs.

To finish this setup run the following command inside of emacs to
download and install some additional required fonts:

``` shell
M-x all-the-icons-install-fonts
M-x nerd-icons-install-fonts
```

[1]: https://github.com/jwiegley/use-package
[2]: https://github.com/railwaycat/homebrew-emacsmacport
[3]: https://www.jetbrains.com/lp/mono/
[4]: https://github.com/radian-software/straight.el
[5]: https://github.com/jdtsmith/emacs-mac/
[6]: https://www.latofonts.com/
