---
title: Truncating git history
tags: howto, git, old-post-is-old
location: Syndey, New South Wales
excerpt: 
  Sometimes you just want to throw away all the early history of a git
  repository. This is how to do it.
---

> It's the beginning of a new year so I'm cleaning out some files in my
> drafts directory. This post was started on 18 July 2012.

Sometimes you don't actually want to keep the entire history of a
branch in a git repository. If it's just one or two commits you want
to discard then `git rebase` and `git filter-branch` can be
helpful. If you just want to drop every commit before some arbitrary
point then you can do the following.

I want to rewrite the `master` branch to discard all commits before
`e41d7f633c45c46bd42e97cecf93204191d9e4c9`. My new history will take the
tree of this commit as its "inital commit". The process is simple:

1. Make a new branch (I'll call it `temp` below); and

2. Use `rebase` to replay every subsequent commit on top of the new
   branch.

````.bash
git checkout --orphan temp e41d7f633c45c46bd42e97cecf93204191d9e4c9
git commit -m "Truncate history"
git rebase --onto temp e41d7f633c45c46bd42e97cecf93204191d9e4c9 master
````

