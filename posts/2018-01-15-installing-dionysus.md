---
title: Installing Dionysus on OS X
location: Melbourne, Australia
tags: Python, howto, software, install
excerpt: 
  A quick guide to installing the Dionysus 2 Python module on OS X.
---

This is a short procedure to install [Dionysus 2][1] on OS X written
for people who aren't necessarily comfortable installing C++ libraries
and the like.

[1]: http://mrzv.org/software/dionysus2/

We'll install:

1. the C++ compilers and some tools to help manage things.

2. `cmake` - which Dionysus uses to build it's C++ library.

3. `boost` - which Dionysus uses to expose an API in the Python
   programming language.

# Compilers and tools

First let's make sure you have all the compilers installed.

```
xcode-select --install
```

This will either prompt ask you to agree to the license agreement and
download the appropriate package or, if you already have things
installed, display a message like this:

> xcode-select: error: command line tools are already installed, use
> "Software Update" to install updates.

Once you have the compilers installed you can
use [Homebrew](https://brew.sh) to install the other tools and
libraries. Homebrew is a package manager which can install many common
open source packages on OS X.

If you already have the `brew` command (run it on a Terminal and see!)
you can skip ahead. If not, open `Terminal.app` and follow the
instruction in the *Install Homebrew* section of
the [Homebrew website](https://brew.sh/).

# Installing Dionysus and dependencies

Dionysus depends on `cmake` and `boost-python`. You can install them
both with `brew install`.

```
brew update
brew install cmake
brew install boost-python
```

Now with the dependencies installed you should be able to install
Dionysus with the normal Python installation tools:

```
pip install dionysus
```

# Making sure it works

You should now be able to run the `python` command to get a Python
prompt and run the `import dionysus` statement as described in the
Dionysus tutorial:

```
[thsutton@laptop TDA]$ python
Python 2.7.10 (default, Oct 23 2015, 19:19:21)
[GCC 4.2.1 Compatible Apple LLVM 7.0.0 (clang-700.0.59.5)] on darwin
Type "help", "copyright", "credits" or "license" for more information.
>>> import dionysus
>>> print dionysus.__version__
2.0.4
>>> exit()
[thsutton@laptop TDA]$ python
```

Installing Dionysus version 1.0 is quite a bit more fiddly. If you
find these instructions helpful, I would suggest just using version
2.0 as installed here.
