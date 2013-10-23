---
layout: post
title: Rewriting git commit authorship
tags: git, howto, rewrite, history
location: Sydney, New South Wales
excerpt: 
  This is a short note about rewriting the history of a git repository to
  change the commit authorship.
---

Here's a quick guide to rewriting the history of a git repository to correct
commit authorship. I've found this useful when committers use multiple machines
with different `user.name` and `user.email` configurations and they need to be
brought into line.

The first example is useful to "fix" commits which have been made with the
wrong author name and email. I've sometimes seen this when people use git for
deployment and make an "urgent" fix on a production servers and commit as
"web-owner@web3.example.com".

Rewriting all commits that have email `web-owner@web3.startup.com` to have the
name "Thomas Sutton" and e-mail "me@thomas-sutton.id.au" is simple:

````{.bash}
git filter-branch --env-filter 'if [ $GIT_AUTHOR_EMAIL = web-owner@web3.startup.com ]; then GIT_AUTHOR_EMAIL=me@thomas-sutton.id.au; GIT_AUTHOR_NAME="Thomas Sutton"; fi; export GIT_AUTHOR_EMAIL GIT_AUTHOR_NAME'
````

If this is a regular problem for you, you might want to rewrite all commits
with an email address that *does not match* some pattern. If your pattern can
be written as a glob you can use the built-in pattern matching functionality in
`bash`:

````{.bash}
git filter-branch --env-filter 'if [[ $GIT_AUTHOR_EMAIL != *@examplecorp.com ]]; then GIT_AUTHOR_EMAIL=tech@examplecorp.com; GIT_AUTHOR_NAME="Example Corp"; fi; export GIT_AUTHOR_EMAIL GIT_AUTHOR_NAME'
````

Here I'm rewriting every commit where the email address *does not* match
`*@examplecorp.com` to have the name "Example Corp" and email address
`tech@examplecorp.com`. Notice that this example uses *double* square brackets
around the condition: `[[ ]]`. It's this change that enables pattern matching
as opposed to the single brackets and simple equality in the first example.

You need to remember, though, that techniques like this rewrite the complete
history of the repository. This means that all other branches, all clones,
etc., etc. will all need to be re-done to match with this new history. Do *not*
do this unless you know what this warning means and how to resolve any issues
you'll have.
