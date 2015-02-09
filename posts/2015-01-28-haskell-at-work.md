---
title: Haskell at Work
strapline: Using Haskell in the Real World at my new job.
tags: haskell, functional programming, work
location: Sydney, New South Wales
excerpt: 
  I've recently (in blog years) started a new job at a company which uses
  Haskell quite extensively. Here are some things we do with it.
---

I recently (nearly 7 months ago) started work at [Anchor Hosting][anchor] as
a software developer in the engineering department. Anchor is a hosting
company, and its engineering group works on a variety of different things like
time-series data storage and analysis, APIs and automation, business
applications, and development tools.

[anchor]: https://www.anchor.net.au/

I'll describe some of our development tooling in this post and leave discussion
of the "real" systems we work on for a later post.

Building and deploying
----------------------

Given we're a Haskell shop, we've needed to figure out how to build, deploy,
and manage systems written in Haskell. I'm still not sold on the whole
containerisation mania which seems to be sweeping certain parts of the IT world
(and certainly not for general purpose multi-tenancy). Nevertheless, we're
finding [Docker][docker] quite useful.

[docker]: https://www.docker.com/

We have a Docker image containing GHC and cabal configured to use the
[Stackage][stackage] package set, with a bunch of frequently used Haskell
packages already installed. This image is built automatically using a [small
set of scripts][d-haskell].

[stackage]: http://www.stackage.org/
[d-haskell]: https://github.com/anchor/docker-haskell "Build a Docker image with GHC and Stackage"

Most of the systems we build operate as services (HTTP servers, agents on
message queue, etc.) and we "package" them as Docker images too. We have
[another set of scripts][d-build] which use the Haskell image to build
a cabal package, extract the artefacts, and stuff them into a new Docker image.
This approach results in an image which is significantly smaller than it
otherwise would be.

[d-build]: https://github.com/anchor/docker-build "Build a Haskell package and make a Docker image of it."

Both sets of scripts can be used manually but they are also used in
[Jenkins][jenkins] jobs. Like everything mentioned so far Jenkins and its
builders all run in Docker too, so we also have some scripts to build [Jenkins
Docker images][d-jenkins].

[jenkins]: http://jenkins-ci.org/
[d-jenkins]: https://github.com/anchor/docker-jenkins "Build Docker images for Jenkins servers and builders."

All of these Docker images are run on [CoreOS][coreos] servers hosted on
[Anchor OpenCloud][aoc], Anchor's new [OpenStack][os] deployment. Generally, we
run each service as a Docker container managed by a `systemd` unit with its
configuration and data files (such as they are) mounted in from the host file
system. Each `systemd` unit deletes any old container and pulls the latest
image before starting the service, so upgrading an instance is easy: just
restart the `systemd` unit.

[coreos]: https://coreos.com/
[aoc]: http://www.anchor.com.au/opencloud/ "Ultra-high performance, public OpenStack cloud."
[os]: http://www.openstack.org/

Development tools
-----------------

Various members of the team all use different operating systems (various
flavours of BSD, Linux, and Mac OS X), editors (vim, emacs, Sublime Text) and
have different opinions and habits about coding style, etc. To help manage
improve the consistency and, hopefully, quality of our code, we developed
[git-vogue][gv]. `git-vogue` runs as a pre-commit hook (currently for `git`,
but it can be extended) and runs a range of checks over the modified or,
optionally, all files in the repository. This isn't perfect but has helped
improve our code quite considerably.

[gv]: https://github.com/anchor/git-vogue/
