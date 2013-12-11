---
title: Inaugural Sydney Elasticsearch Meetup
tags: elasticsearch, search, meetup, event, golang, monitoring
location: Sydney, New South Wales
excerpt: 
  The inaugural Sydney Elasticsearch Meetup had talks on monitoring
  Elasticsearch clusters and on the impending 1.0 release.
---

The inaugural [Sydney Elasticsearch meetup][1] at Atlassian (who provided the
space, beer, and pizza) featured two talks:

- [Sharif Olorin][2] from Anchor systems spoke about monitoring Elasticsearch
  clusters; and 

- [Clinton Gormley][3], a core developer, gave an overview of the changes in
  the impending 1.0 release.

[1]: http://www.meetup.com/Elasticsearch-Sydney-Meetup/events/149068632/
[2]: http://tesser.org
[3]: https://twitter.com/clintongormley

# Sharif Olorin on Monitoring Elasticsearch

Sharif is a developer and system administrator at Anchor Systems and has been
working with Elasticsearch for about a year.

He highlighted a number of key points to be considered by anyone who is
monitoring an Elasticsearch cluster (in no particular order):

You should **monitor every metric** that you can get your hands on and keep as
much data for as long as you can, just in case. Sharif described several cases
where having data available made debugging problems observed in production much
easier.

While you should *monitor* everything, you should **only alter on metrics
people care about**. Being woken up at 3AM is pretty bad, but it's worse when
the cause is not really a problem! Loss of redundancy, for example, probably
isn't worth getting out of bed for; provisioning a new node can wait until
morning.

You should monitor and **alert from your entire cluster**, not just from some
node's individual opinion about the whole cluster. There are a number of
problem conditions that can be difficult to accurately detect without having a
"whole cluster" view. Whole-cluster monitoring, though, doesn't play nicely
with most host-based monitoring tools; you'll probably need to define your own
custom checks which know how to interrogate the whole cluster.

Given that they'll be checking every node in a cluster, these checks will need
to be highly concurrent and very fast. Sharif showed us a split brain check he
wrote in Golang.

You should **automate recovery** from as many alert conditions as you can.
Where it's not possible to automatically *recover* from an error condition, you
should aim to *respond* sensibly. Sharif described an example, in which an
alert trigger by a split-brain in the cluster might automatically switch all
nodes into read-only mode to prevent divergence.

**Use the statistics API** as a source of many useful metrics about your nodes
and their opinions about the cluster. It also exposes a bunch of generic stuff
about the JVM and things.

Finally, Sharif gave a few tips about *tuning* Elasticsearch clusters. The core
of his advice boiled down to taking a principled approach: tuning individual
parameters, reproducing each test as closely as possible (same time of day,
etc.), consider *actual numbers* (not just graphs), etc. Essentially: use
science!

[Sharif's slides][4] are available on Speaker Deck; I've probably got a bunch
of stuff wrong here, so you should probably go and review them for yourself!

[4]: https://speakerdeck.com/fractalcat/monitoring-elasticsearch-for-fun-profit-and-not-getting-woken-up-at-3am

# Clinton Gormley on Elasticsearch 1.0

Clinton works for Elasticsearch where he develops the Perl client libraries,
does training and evangelising, "keeps Elasticsearch honest" and some other
stuff. He gave us a run down on the new features and other improvements in the
forthcoming Elasticsearch 1.0 release.

Amongst the many things mentioned, these few stuck out to me:

You cannot currently use different versions of Elasticsearch in the same
cluster; upgrades involve tearing down your entire cluster and bringing up a
new one (possibly not in that order). 1.0 will allow **rolling upgrades** of
your nodes without having to do the whole cluster in one fell swoop.

You *can* **backup** the data in current Elasticsearch clusters, but it's very
much a do-it-yourself process: disable flushing, find all primary shard
locations, copy their files, enable flushing. Version 1.0 will provide API
methods to trigger a snapshot which will be written to a configured
*repository* (S3, HDFS, etc.) Comparable changes have been made to the process
of **restoring** a snapshot: the current manual process will be replaced with a
few API calls.

The **percolator** functionality -- which allows applications to do things like
reverse search, alerts, and updatable result sets -- is now implemented in a
way which lets it scale as well as any other index in the cluster. It also
supports multiple indices, aliases, and has a bunch of other improvements.

The new **cat API** provides direct access to a range of metrics in
human-friendly formats (i.e. not large JSON documents). This includes a bunch
of things that humans and monitoring systems are often curious about: sizes,
counts, statuses, etc.

The existing support for facets has been drastically improved with a new
feature called **aggregations**. These allow you to express a bunch of things
which aren't really expressible with traditional facets. These looked very
powerful and very cool!

[Clinton's slides][5] (originally prepared by Igor Motov) are available on
Speaker Deck; go read them!

[5]: https://speakerdeck.com/elasticsearch/new-features-in-elasticsearch-v1-dot-0

The questions prompted a few interesting details: Elasticsearch have just hired
a few machine learning people to work on the product; they aren't yet sure
what, specifically, they'll be working on, but we can expect some learning-type
things in Elasticsearch soon.
