---
title: Emacs, Hunspell, Dictionary, and You
location: Sydney, New South Wales
tags: howto, emacs, spellcheck, dictionaries
excerpt: 
  Here's how I (finally) got emacs to use hunspell on OS X with the
  correct dictionary.
---

Emacs is hard to use and even harder to learn how to use. Here is what
I (finally) had to do to make emacs spell-checking work on OS X with:

1. [Hunspell][1]; and
2. an [Australian English Dictionary][2].

It's pretty easy once you know what to do:

1. Install hunspell.

    ````
    brew install hunspell
    ````

2. Download and install the dictionary files:

    ````
    mkdir -p "$HOME/Library/Spelling"
    wget 'https://downloads.sourceforge.net/project/aoo-extensions/1232/7/dict-en-au-2008-12-15.oxt'
    unzip dict-en-au-2008-12-15.oxt *.aff *.dic -d ~/Library/Spelling
    ````

3. Edit your `~/.emacs` file to add the following (or something
   morally equivalent):

    ````{.scheme}
    ;; Set $DICPATH to "$HOME/Library/Spelling" for hunspell.
    (setenv
      "DICPATH"
      (concat (getenv "HOME") "/Library/Spelling"))
    ;; Tell ispell-mode to use hunspell.
    (setq
      ispell-program-name
      "/usr/local/bin/hunspell")
    ````

   You'll probably need to add something else to make spell-checking
   actually happen. I add `flyspell-mode` to the hooks for the major
   modes I use to edit things that need spell-checking: Haskell source
   and Markdown text files. You can see examples of this in
   [my `.emacs` file][3].

Now try to edit something using emacs. Type some gibberish to make
sure it works and try some dictionary specific words to make sure it's
using the right dictionary (I use "colour" to make sure it's not using
an en_US dictionary).

As a side note, the only reason I went on this saga is because
hunspell can't find dictionaries installed in `~/Library/Spelling`
unless you happen to started it from your `$HOME` directory:

````
$ pwd
/tmp
$ hunspell -D
SEARCH PATH:
....:Library/Spelling:....
AVAILABLE DICTIONARIES (path is not mandatory for -d option):
Can't open affix or dictionary files for dictionary named "en_AU".
````

The `....` ellipsis is where I've omitted quite a few relative and
absolute paths but most of them are paths that some versions of Open
Office got installed to by some installers at some point in time. It
looks like [hunspell attempts prefix these paths with `$HOME`][4] but
it doesn't seem to work for some reason (`$HOME` is defined and
non-empty).

If only autoconf 2.69 and automake 1.15 were able to prepare the
hunspell source tree to building I'd take a close look at this C/C++
string munging and submit a pull request. (It's 2017! Surely we can ship
release tarballs with a configure script?)

Alas.

[1]: https://hunspell.github.io/
[2]: http://extensions.openoffice.org/en/project/english-australian-dictionary
[3]: https://github.com/thsutton/dotfiles/blob/master/.emacs#L94
[4]: https://github.com/hunspell/hunspell/blob/master/src/tools/hunspell.cxx#L2056
