---
title: Installing Python 3.5.1 from source
tags: howto, linux, software, install, source, python
location: Sydney, New South Wales
excerpt: 
  A quick introduction to installing software (like Python) from
  source on Linux.
---

This is an introduction to building and installing software from
source on Linux (and other UNIX-like operating systems). The example
is Python 3.5.1 but the details apply to the vast majority of open
source software. I don't cover everything - mostly just the happy path
of everything working - but if you've never installed from source
before, I hope this will help guide you through the process.

Speaking of process, here is what we'll be doing:

1. Download the source code of an official Python release.

2. Configure the build appropriately for our machine.

3. Compile the software.

4. Test the software to make sure it works properly.

5. Install the software.

6. Configure our system to make sure we can use the software easily.

Getting a Python release
========================

Go to the [Python source downloads][1] and choose a version to
download (I'll use 3.5.1 throughout this document but the process
should be similar for any other version). Download the "Gzipped source
tarball" to your server. You can do this by copying the URL in your
web browser and using the `wget` command like so:

````{.bash}
wget https://www.python.org/ftp/python/3.5.1/Python-3.5.1.tgz
````

[1]: https://www.python.org/downloads/source/

This will download the release and save it in a file based on the
URL. In our case it'll be called `Python-3.5.1.tgz`. If you don't have
`wget` installed (maybe you get a `command not found` error) you can
install it using your OS package manager:

````{.bash}
sudo yum install wget
````

Once the file is downloaded you can unpack it using the `tar`
command. Originally `tar` was used with magnetic tape drives (the name
stands for tape archive) but it also works for archives stored in
plain old files on disc and it's still the standard archive format in
UNIX environments. `tar` is often used together with `gzip`
compression to reduce the size of file but all modern `tar` commands
can handle compression and decompression transparently for you. `tar`
archives usually have the `.tar` file extension and often have another
extension for the compression format like `.gz`, `.bz2`, or `.xz` (for
GZipk, BZip2, or XZ respectively). These extensions can be piled on
top of each other like `.tar.gz` or squashed together into a single
extension like `.tgz`. You can *extract* a tar archive *file*
using the following command:

````{.bash}
tar xf Python-3.5.1.tgz
````

`tar`'s argument handling is ancient and unlike almost all other UNIX
programs so you probably needn't bother learning how the work; just
remember `tar xf` means "e**x**tract from a **f**ile". Like most UNIX
programs, `tar` will not produce any output unless it encounters an
error or you specifically ask it to. If you'd like it to give you some
feedback as it works use `tar xvf` instead ("e**x**tract **v**erbosely
from a **f**ile").

You should now have a directory called `Python-3.5.1/` which contains
the release files. From now on I'll assume we're working inside this
new directory:

````{.bash}
cd Python-3.5.1/
````

Building Python
===============

Most UNIX software uses a very similar build process:

- first you *configure* the build with the install location, where to
find any libraries it needs, etc.

- then you *compile* the software

- then you *test* the software

- then you *install* the software

Each step is usually a single command and the whole process usually
looks a little like this:

````{.bash}
./configure
make
make test
sudo make install
````

The `./configure` command runs the *configure script* which is
included in most software packages. This is usually an enormous,
automatically generated shell script which figures out how to call
your compiler, where to find the libraries the software needs, where
the new software should be installed, etc. If `./configure` cannot
find a compiler, a required library, etc. then it will report an
error. You can usually tell which command or library is missing from
the error message but it can be tricky to tell what to do to fix the
problem. Usually it's a missing library or header file and you can
install the, e.g., `foo` and `foo-devel` packages and try to run
`./configure` again. When I'm feeling too lazy to read the
requirements for a package it's not unusual to go through this cycle a
few times.

One of the things `./configure` produces is another large,
automatically generated script called `Makefile` (often there will be
several of these scattered throughout the subdirectories of the
software). The `Makefile` contains instructions on how to achive
various tasks like "build" or "make the executable called `python`" or
"run the test suite". The `Makefile` script is executed with the
`make` command. You can run `make` to execute the default task
(i.e. the first one in the file) or, e.g., `make test` to run the
"test" task.

The default task for Python, like most packages, is to compile the
software so running:

````{.bash}
make
````

should kick off the build process. For a widely used, well tested
package like Python you should expect `make` to succeed whenever
`./configure` does.

Once the build is finished you should run the test suite to make sure
that it all works as expected:

````{.bash}
make test
````

Again, official Python releases are exceptionally reliable and the
tests can take a long time to run so you might like to give this a
miss.

Installing
==========

Once it's built you should install the software into some permanent
location. Almost all packages that follow the `./configure`, `make`,
`make install` convention will install under the `/usr/local/`
directory by default: they'll put their executables in
`/usr/local/bin/`, data files in `/usr/local/share/`, etc. The
`/usr/local/` directory is properly considered part of the operating
system (as opposed to part of your user account) so you should require
`root` privileges to create files and folders there. This is why `make
install` us usually run under `sudo`:

````{.bash}
sudo make install
````

This will create the directories and copy the compiled files into
place as the `root` user.

You might like to install it somewhere else. Perhaps you don't have
`root` access or you'd like to install multiple versions side-by-side?
In this case you'll need to change the `./configure` command you run
at the start to have a `--prefix` argument:

````{.bash}
./configure --prefix=/some/other/directory
````

If you've already used `./configure` and/or `make` with the default
settings you can use `make clean` to wipe out any work it did and
start again. For example, let's install into the `py-351` directory
under your account home directory you could run the following command:

````{.bash}
make clean
./configure --prefix=$HOME/py-351
make
make test
make install
````

Note that you don't need (and shouldn't use) `sudo` to run `make
install` this time because you already own your `$HOME` directory. If
you run `ls $HOME/py-351` you should see all the installed files.

Using the installed software
============================

Now that the software is installed there are a few ways you can use
it; in order of difficulty:

- Specifying the full path

- Changing your `$PATH` temporarily

- Changing your `$PATH` permanently

You can always run a command simply by *specifying the full path* to
the executable to run. Following on from the `$HOME/py-351` example
above:

````{.bash}
$HOME/py-351/bin/python3 -V
````

should run the newly installed Python interpreter and output the
version message.

If you want to use the new install in *just this shell session* you
can change your `$PATH` temporarily:

````{.bash}
PATH="$HOME/py-351/bin:$PATH"
````

The `$PATH` variable contains a `:` separated list of directories to
search for an executable when you run a command in the shell.  The
first executable file with the right name is the one that will be
executed. The `which` command can do this search and tell you which
file will be executed or even give a list of all the options:

````{.bash}
which python3
which -a python3
````

Notice that we are set `PATH` to be our installation directory
followed by the current value of `$PATH`? If we leave the `:$PATH` off
the end we'll *replace* our existing `$PATH` and the shell will be
able to find *only* our newly installed Python commands!

If you want your new `$PATH` setting to be used by all programs that
get started by you shell session (i.e. not *just* the commands you
manually type in but ones that they start in turn) you'll need to
`export` the variable:

````{.bash}
export PATH
````

For the rest of your current shell session every mention of `python3`
in the commands you type in or the commands that programs you run
execute for you mean `$HOME/py-351/bin/python3` (if that file exists).

To make this change permanent you need to change your shell
configuration files. The name of these configuration files and whether
or not they already exist can change from system to system but they
are always in your `$HOME` directory and are called one or more of the
following:

- `.bashrc`
- `.bash_rc`
- `.bash_profile`
- `.profile`

If any of those files already exist in your `$HOME` directory (files
beginning with a `.` are considered "hidden" by most UNIX tools,
you'll need to use `ls -a` instead of `ls` to see them) then you
should edit that file. If not, `.profile` is probably a safe bet.

Create or open the chosen file in a text editor (I'll describe using
`vi` or `vim`) and add the lines you used above to the *end* of the
file:

````{.bash}
PATH="$HOME/py-351/bin:$PATH"
export PATH
````

To edit a file with `vim` (or `vi`, for our purposes they are
identical) just pass it the name of the file as an argument:

````{.bash}
vim .profile
````

Unlike most other editors `vim` has *modes*. The most important modes
are *command mode* (where you give the editor commands like "save" or
"open a new file" or "quite") and *insert mode* (where you insert text
into the file). By default `vim` starts in command mode but you can
enter insert mode by pressing the `i` key. When you are in insert more
you should see an `-- INSERT --` on the bottom line of your
terminal. While you are in insert mode you can use the keyboard arrow
keys to navigate and any characters you type should be inserted into
the file. Scroll to the bottom of the file and enter the two new lines
as before. To leave insert more press the escape key on your keyboard;
the `-- INSERT --` should go away. There are plenty of keyboard
shortcuts you can use in command mode but the easiest way to enter a
command is with the command line which appears when you type a
`:`. When you do you should notice the cursor jump to the bottom line
of your terminal after a `:` character. The command to write your
changes to disc and quit `vim` is `wq` (just like the shell you end a
command by pressing the enter key).

You should now be able to restart your shell and, if all goes well,
run the `python3` command without specifying the full path
`$HOME/py-351/bin/python3`. If it does not work after starting a new
shell (e.g. starting a new terminal or logging in again) try looking
at the output of `echo $PATH`. If it does not have new value at the
start your shell is probably using one of the other files I listed
above. Try making the same changes to each of them until it does work.

In this section I used the `$HOME/py-351` prefix but all of these
steps will work for the `/usr/local` prefix too and, on many systems,
you *will* need to do them.
