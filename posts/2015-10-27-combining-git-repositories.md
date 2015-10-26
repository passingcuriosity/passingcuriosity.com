---
title: Combining git repositories
tags: git, howto, rewrite, history
location: Sydney, New South Wales
excerpt: 
  A quick run down of combining git repositories which do not share history.
---

Rewrite the commit messages on the 'rewrite' branch to mention the
project name:

```.bash
git filter-branch --msg-filter '/bin/echo -n "Nirvana: " && cat'
```

Rewrite the commit trees on the 'rewrite' branch to move the files
into the project directory:

```
git filter-branch --tree-filter 'mkdir -p nirvana &&
	find . -mindepth 1 -maxdepth 1 ! -name nirvana -exec mv {} nirvana
	\;' HEAD
```

This command creates the `nirvana/` directory (if it does not already
exist) and then moves all files and folders in the current directory
into `nirvana/` (excepting `nirvana` itself). A better filter command
(if it works; I haven't tried it) would be:

```
mkdir -p nirvana; mv * nirvana; true
```
