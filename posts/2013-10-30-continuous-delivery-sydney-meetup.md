---
title: Sydney Continuous Delivery Meetup
tags: continuous delivery, atlassian, stash, git
location: Sydney, New South Wales
excerpt: 
  The October Continuous Delivery meetup had talks about the continuous
  delivery practices of the Atlassian Stash team and about automated
  environment provisioning with CloudFormation and Puppet.
---

# Atlassian Stash

Matthew Watson leads the Stash team at Atlassian. Released a minimum viable
product *really* early, then very short delivery cycles; provide incremental
improvements to both deliver features to customers but also to validate the
product ("we *will* catch up").

Agile with git. Use feature branching to allow for isolated development with
functional and performance testing of *your* code as you develop it. Isolate
stable code from work-in-progress.

Code reviews (aka pull requests) to help ensure higher quality. Needed to
inculcate a respect for quality in the team; absolutely required for continuous
deployment.

Every commit in master results in two builds and pushes the result to the
"dog-fooding" deployment. Also runs a bunch of other checks against these
successful builds.

Whenever a feature branch is created, CI server creates a new "CI plan" for
that branch. These feature branch jobs are quite highly optimised compared to
the master.

Several build styles:

- "Checks" builds use checkstyle, findbugs, API compatibility, licensing, link
  check docs. This is pretty fast; 4-5 minutes.

- "Master" builds, every green build is deployed to dog-fooding instance.
  Parallel; distribution, functional tests (40 minutes), jsunit, jshint, DB
  migration, REST and hosting testing, first run, unit tests. Major gatekeeper
  (~11 workers)

- "Dependent" builds: database matrix (12 versions of 5 DBs), git version
  matrix (12 versions), plugins, source, git on Windows, hosting and REST on
  Windows. Triggered when "master" succeeds; so, e.g., Windows errors don't
  prevent getting code into master.

- "Feature branch" builds include "checks", Stash distribution, unit tests,
  jshint; these are all pretty fast (~4 agents, 3-4 minutes; fast feedback to
  developers). Developers can trigger second stage with all the other test from
  "master" if they think they're required.

## Release branching

Stash needs to support multiple releases:

- master

- release branches (2.5, 2.6); lives on with bug fixes, etc.

- merge bug-fixes back toward and into master (in 2.5, merge into 2.6, then into
  master). Plugin to do this sort of stuff automatically, create pull requests
  for merge conflicts.

Master and release branches all get full build plans.

## Release build

Fair number of builds made internally.

- Stash team have internal dog-fooding instance.

- Atlassian has a corporate instance used by other teams.

Use a release job, parameterised by a specific commit (not necessarily the head
of a branch). Pass in version and next-version for Maven. The build job runs
`maven release:prepare release:perform`. Not much testing because it's already
been through all the testing.

Create a temporary branch for the process of the release build.

## Automated deployments

Release artefacts for customers:

- www.atlassian.com, developers.atlassian.com
- marketplace
- Checkup (for third-parties to test their plugins)
- Go live

Internal deployments:

- Dev staging

- Staging (same data as stash.atlassian.com) for smoke tests, etc.

- Production

## Performance testing

Six-thousand seat licenses with lots of CI running against it. Performance is
pretty important.

Daily monitoring of performance of operations. Check results in stand ups.
Notice regressions in performance.

## Techniques

Try to always be ready for release - quality code.

Automate testing as much as possible.

Automate processes:

- Releases

- Deployment

# Automated environment provisioning

David Cheal is Chief Engineer at [Krunchtime IT](http://krunchtime.it). They do
AWS solution design, build and delivery; devops approach; agile; etc.

Life cycles are changing 

- Traditional ops environments

- Virtualisation for cost efficiencies

- Cloud

None of the problems we had at (1) are still there at (3), we've just given
that problem to Amazon.

## Continuous delivery infrastructure

Deploy infrastructure often, automation, etc. Help to detect and prevent
configuration drift between environments, etc.

## Options for infrastructure agility

- CloudFormation with baked AMIs

- Automation tools like Puppet or Chef to automate configuration and deployment.

## Separation of concerns

Code deployments are not infrastructure deployments. Leave application and
dependencies to infrastructure deployment.

## Continuous challenges

Technological challenges: Windows, anything from MS, learning curves

Cultural change: change, resistance, etc.

## Example

Communities can be a guide; Puppet Forge is a guide but they *must* be reviewed.

## Pragmatism

> Infrastructure is code.

This is not true; infrastructure is infrastructure.

## Opportunity

Embrace new methods.

Incredible opportunity to deliver what business, operations, and developers
need more quickly, reliably, cost effectively, etc.

## Demonstration

Simple Ruby on Rails application using AWS CloudFormation to spin up EC2
instances, RDS database, ELB, etc. Use an existing Puppet master.

Most organisations use a naming convention which allow Puppet to determine
which classes should be applied to each server, with the right environment.

Using ELB, most organisations will include a specific "health check" page in
their application. Makes it very easy to have ELB.

Continuous infrastructure delivery is -- when done right -- really, really
boring.

## Cloudiness

Elasticity lets you scale according to demand. "Deal of the day" sites 2-15-2
machines. Development infrastructure shutdown overnight and weekends.

Immutable, disposable infrastructure. Cows vs dogs.
