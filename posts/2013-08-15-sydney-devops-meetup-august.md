---
title: Sydney Devops Meetup, August 2013
tags: devops, release management, services, java, erlang, red hat, knife, chef
location: Sydney, New South Wales
toc: display
excerpt: 
  A talk on release management at Yahoo!7 and lightning talks on plain old
  services, Erlang and Elixir, the RedHat Summit, and support tooling with
  Knife.
---

Here are some nodes from the [August 2013 Sydney Devops Meetup][event].

[event]: http://www.meetup.com/devops-sydney/events/117291642/

# Artur Ejsmont on release management at Yahoo!7

Artur is a Senior Software Engineer at Yahoo!7. I think he said he's on the
platforms team? The environment within the team is rather different environment
than many others -- much more in common with release engineering and system
administration than in other roles.

Everything is released and deployed as packages using a suite of tools and
formats developed with the Yahoo! empire. Packages include (almost) everything:
PHP source code, crontabs, configurations, etc.

Release descriptions (CMR) include:

- package versions and clusters
- conf and cron changes
- database and process management

## Joined Team

When he joined the team, the 5 members were responsible for 180 packages
(committing to 1-2 dozen packages in an average sprint).

There was a lack of visibility in not only the state of various packages
(deployed versions, build and test status, etc.) but even which packages there
*are* (commited to SVN but never made it into the package repository).

Problem with packages lingering without stable releases. Wanted to be able to
recreate environments, etc. but dependencies not being promoted to stable can
make it a pain in the arse to track down specific versions.

- Uncertainty what has to be released

A great deal of manual work to assemble change management requests for
releases. Two days of work at the end of each sprint, trawling through
documentation, trackers, SVN, etc.

Ten different application clusters with different versions of different
packages on each.

- Manual testing of int stage prod

Perception was that the team was doing way too much manual work.

Constantly searching for information in disparate sources; repos, code,
trackers, wikis, etc.

Ecosystem is too complex.

Too many moving parts & chances to screw things up.

## Vision

Provide visibility

> I don't want to guess, nor search.

Automate

> Do it for me or telll me what to do next.

Data aggregation

Single point of entry for Bugzilla, svn, ci, dist, CMR tool, etc.

Provide metrics

## Development

Built it over Christmas period.

1. Automated job to prcoess entire SVN repo, discover packages and generate 190
static HTML reports.

2. Second release using MySQL.

## Package List

List of 190 packages. Sort by: CI state (broken at top), release state (commits
but no version released), package created (but not deployed everywhere yet), up
to date.

Provides information including:

- Version numbers (svn trunk, newest in package repo, oldest in production)

- "Score" (higher is worse) so it can fudge things by priority.

- Links to various sources of information (related CMRs, SVN, CI, repo)

Rollup

- Healthy
- Pending
- Unhealthy


## CMR Builder

Interrogates various data sources:

- SVN
- Igor (server role manager)
- Repository (dependencies)
- Deployments

Assemble changelogs, etc.

Some packages are based on old CVS repositories, need crazy date-based logic to
build a diff.

## Dependencies

Dependencies between packages are really annoying; lots of dependencies between
packages. 10 major applications, 190 packages. Only a few packages are
relatively independent.

Provides overview of dependencies:

- List of packages required by this package
- List of packages which require this package

## Metrics

Metrics to tell:

- How are things? Good or bad?

- How are things changing? Getting better?

Lag-Score includes a range of factors (tests failing, production versions,
etc.) which tries to combine all the factors. Plotted, making very little
progress on this over 6 months.

## Questions

Why a custom packaging tool?

> It was invented at Yahoo! before there were existing tools like dpkg,
> rpm, etc. Lots of tools to manage, e.g., 40,000 servers involved in
> Yahoo! Mail.
>
> Given the tools and scale, it probably won't be going away.

Release notes: if it's bullshit, why not kill it completely?

> It's an embedded part of the environment and culture of this team and
> other teams. Also: comes from global.
>
> CMRs provide communication channel between teams and sysadmins. It's a
> heavy process, and are trying to make it more lightweight, but safety
> is important.

How fast do you go?

> About two release windows a week.
>
> Sprints are about 3 weeks, but not religious about it.
>
> SCRUM-ish, but no product owner, etc. so only ish.

Have you got your tool into other teams?

> New version is in use by three or four more teams.
>
> Internal presentation, now crawling all the things. Using maintainer
> information to group stuff into teams.

Are all envrionments managed in the same way?

> Yeah, it's all controlled using the same tools.

Reproducing production in staging for incident response?

> Easy using the role-based server management system.

Configuration management in packages?

> Packages declare the configuration options they have.

More

> Command to override value for a configuration parameter declared by a
> package.
>
> Changes to databases aren't managed, managed manually. Sometimes have
> to make schema changes backward compatible and run before hand, etc.

# James Gorman on Plain Old Services

A lot of this is about James having the shits with the way they do things at
Yahoo!7 and on the web in general.

Working in Java, metric shit ton of frameworks. JBoss got deprecated.

> Everything you can do with Tomcat is an awful hack.

Found data intensive server container. Based on Jersey but simple. Also:
focussed on the web. Architecture three tier architecture.

Want more asynchronous: message queues, etc. Decoupling. Wrote a thing that
does this. Similar architecture but more ways of asking for things to be done
(cron, message queues, etc.)

> I don't recommend anyone ever write server middleware.

# Peter Ericson on Erlang and Elixr

[Erlang][] is erlang; [Elixir][] is a ruby-ish language which compiles directly
to Erlang bytecode.

[Erlang]: http://www.erlang.org
[Elixir]: http://elixir-lang.org

Elixir Dynamo is a web framework for Elixir. Scaffolding, etc.

See [example code](https://bitbucket.org/pdericson/erlang_future).

# Sergey Guzenkov on the Red Hat Summit

Been to the US for the [RedHat summit](http://www.redhat.com/summit/) last
month.

They'll be releasing a major new version of [Red Hat Satellite][sat] (their
management thing) building on Puppet, Foreman, Katello, Pulp, Candlepin.

[sat]: http://www.redhat.com/products/enterprise-linux/rhn-satellite/

RHEL7 release is delayed. It'll be based on Fedora 19 and the beta is due in
December 2013. The 7.0 release is expected early next year. Replacing MySQL
with MariaDB; adding MongoDB, nodejs; upgrading a bunch of programming
languages; systemd. Will include client and server support for pNFS -- an
extension of NFS to be parallel.

# Shaun Domingo on making knife and support play nice

Support get queries about rails apps, etc. Ask engineers but they are busy,
etc. Support staff should be able to interrogate things.

Building on top of [knife][] and knifeblock (manage knife configurations).
Plugin allowing support staff to download application keys (to interact with
APIs on their behalf), talk to APIs, generate knifeblock configuration and then
help resolve issues.

[knife]: http://docs.opscode.com/chef/knife.html

````bash
	# List apps.
	knife ninefold-internal -l
	# Generate knifeblock configuration.
	knife ninefold-internal -a 23 -g
	# Activate the knifeblock configuration.
	knife block dev-NF00000004-23
	# Do stuff to help investigate and resolve customer's problem.
	knife ...
````
