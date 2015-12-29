---
title: Combining git repositories
tags: git, howto, rewrite, history
location: Sydney, New South Wales
excerpt: 
  A quick run down of combining git repositories which do not share history.
---

Suppose you have two git repositories which don't share any history
and you'd like to import the files in one into the other. For the sake
of example, I'll call the source repository (the one which contains
the commits I want to import) `nirvana` and the target repository
(the one I want to put the commits into) `midgard`. I'll also assume
that I'm interested in the `master` branch of both repositories.

**Warning** Some of the commands I'm going to use have the potential
to damage the history of your git repository. If you aren't sure what
any of the commands below do you should make a backup of your
repository before proceeding.

The process goes like this:

1. Clone a new copy of `midgard`.
1. Add a remote for `nirvana`.
1. Create a `rewrite` branch based on `nirvana/master`.
1. Rewrite the `rewrite` branch commit messages and file structure.
1. Rebase the `rewrite` branch on `midgard/master`.
1. Push the `rewrite` branch upstream (to PR, review, or whatever).

Preparing a working repository
==============================

I hate having to restore repositories from backup (especially when I
don't *have* backups and have to rely on my local clones) so I always
make a throw-away clone when I'm rewriting history, etc.

```{.bash}
git clone git@example.com:midgard.git import-nirvana
cd import-nirvana
git remote add nirvana git@example.com:nirvana.git
git fetch nirvana
```

These commands give me a new clone of the `midgard` repository, and add a
remote and fetch the `nirvana` repository.

Preparing for import
====================

Once I have the commits it's time to massage them gently into shape
ready for import. In my case, I want to add `Nirvana: ` to the front
of every commit message (so that I can tell which sub-project a commit
changes when I review the `git log`) and move everything in the
`nirvana` repository into a `nirvana` sub-directory (to avoid
clobbering build scripts, etc. which exist in both).

These changes involve rewriting history so I'll do them on a new
`rewrite` branch. If [when] I screw it up the first few times I'll be
able to discard the branch and try again without cloning and fetching
both repositories from scratch.

```{.bash}
git checkout -b rewrite nirvana/master
```

Rewriting the commit messages is conceptually easy but kind of
annoying when using `git-filter-branch`. The `--msg-filter` argument
takes a command which accepts the current commit message on its
standard input and must produce the new message on its standard
output. Using `echo` and `cat` to add "Nirvana: " to the start of the
first line looks like this:

```{.bash}
git filter-branch --msg-filter '/bin/echo -n "Nirvana: " && cat'
```

Moving the files into a subdirectory called `nirvana` is essentially
the same but the command (passed to `--tree-filter` this time) is
quite a bit more complex. Instead of `echo`ing some text and then
`cat`ing the current message we'll `mkdir` a new directory and then
`mv` the files into it.

```{.bash}
git filter-branch --tree-filter 'mkdir -p nirvana &&
	find . -mindepth 1 -maxdepth 1 ! -name nirvana -exec mv {} nirvana
	\;' HEAD
```

I've used `mkdir -p nirvana` to create the directory if it does not
already exist. One thing you need to be careful of when `mv`ing your
files into the new `nirvana` directory is that you don't attempt to
move the `nirvana` directory into itself. If you are using `bash` or
some other "advanced" shell you can enable and use negative globbing
but I prefer to use `find` to select only the files I want to move.

Here `find . -mindepth 1 -maxdepth 1 ! -name nirvana` looks at the
current directory and finds every file (or directory) which is:

- At least 1 level deep (i.e. not just `./`); and
- At most 1 level deep; and
- Not named `nirvana`.

Then each match is substituted for the `{}` in the `mv {} nirvana ;`
and the command is executed. This moves everything *except* `nirvana/`
into `nirvana/`.

Merging branches
================

All that remains is to graft the newly prepared `rewrite` branch onto
the existing `midgard/master` history so that they can be merged
together. Because the tree of every commit in `rewrite` has been
rewritten to have all files under the `nirvana/` directory it should
be trivial to rebase the `rewrite` branch on the `midgard/master`
branch.

```{.bash}
git rebase origin/master
```

This should go through without any intervention. If it does not, you
probably already have a directory called `nirvana/` with some of the
same files as the `rewrite` branch. You'll need to resolve any
conflicts that `git` reports in the same way you usually do but be
sure to use the `git rebase` commands to continue!

With that done the branch is ready to merge but I usually take this
opportunity to `git rebase -i` and squash some commits, update build
scripts to build the imported code, etc. Once this is done I generally
push the branch into the origin repository (`midgard`)

```{.bash}
git push -u review origin/import-nirvana
```

And then open a pull request for my team mates to review.
