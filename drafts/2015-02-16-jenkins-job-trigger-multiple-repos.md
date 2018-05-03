---
title: Jenkins jobs with multiple repositories
tags: jenkins, testing, continuous integration
excerpt: 
  Here's the way I configure Jenkins jobs to run when changes are committed to
  one of multiple repositories hosted on GitHub.
---

Most of my work time in the last month or two has been spent developing a small
constellation of RESTful APIs. The clients, servers, and supporting libraries
for these APIs all live in a single `git` repository hosted on [Github][],
while a suite of acceptance tests lives in another repository. We use
[Jenkins][] to automatically build the software and run the acceptance tests
when changes are pushed into either the software repository *or* the tests
repository.

[Github]: https://github.com/
[Jenkins]: http://jenkins-ci.org/

Setting up a Jenkins job to rub a build like this is pretty easy, but there are
a few pieces to pull together.

First you'll need to install a few Jenkins plugins:

- The [Git Plugin][] is necessary to work with git repositories;

- The [GitHub Plugin][] is required to integrate with the GitHub API;

[Git Plugin]: https://wiki.jenkins-ci.org/display/JENKINS/Git+Plugin
[GitHub Plugin]: https://wiki.jenkins-ci.org/display/JENKINS/Github+Plugin

The next step is to login to GitHub and generate an OAuth token for your
Jenkins server.
