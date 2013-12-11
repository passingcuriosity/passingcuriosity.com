---
title: Inaugural Sydney Elasticsearch Meetup
tags: elasticsearch, search, meetup, event, golang, monitoring
location: Sydney, New South Wales
excerpt: 
  The inaugural Sydney Elasticsearch Meetup had talks on monitoring
  Elasticsearch clusters and on the impending 1.0 release.
---

The inaugural [Sydney Elasticsearch meetup][1] at Atlassian featured two talks:

- [Sharif Olorin][2] from Anchor systems spoke about monitoring Elasticsearch
  clusters; and 

- [Clinton Gormley][3], a core developer, gave an overview of the changes in
  the impending 1.0 release.

[1]: http://www.meetup.com/Elasticsearch-Sydney-Meetup/events/149068632/
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

# Clinton Gormley on Elasticsearch 1.0

Clinton works for Elasticsearch where he 

ES around 0.4 in 2010.

## Rolling upgrades

Currently cant have clusters with mixed versions. Need to bring up a new
cluster and down the old.

## Snapshot and restore (backup)

Currently a bit fiddly:

1. Disable flush
2. Copy data files
3. Enable flush

Now controlled by API. Supports shared file system storage "repositories"
accessible by all nodes (S3, Glusterfs, etc).

Restore is similarly fiddly

1. close indices
2. find shards
3. replace files
4. open indices
5. wait for recovery

Also exposed through API now (though still involves closing indices)

Lucene's immutable file structure will make snapshots efficient

## Percolator

reverse search
updated search results

Register queries with percolator index, submit documents and get back list of
matching queries.

Current approach is linear in the number of queries (in memory index, run each
query); 1.0-beta1 improves percolator:

- based in an arbitrary index
- gives distribution
- existing documents
- counting
- highlight
- facets
- bulk API

## Cat API

Humans are crap at reading JSON documents.

exposes information under `/_cat/` in human-readable formats: allocation,
nodes, shards, master, etc.

## Aggregations

Had facets for a long time. Works very well, but want more.

Aggregations were added to make more sophisticated combined facets.

Buckets group things together, Calculators operate on values in buckets. Seems
directly comparable to `GROUP BY` and aggregate functions in SQL with nested
queries.

Probably in beta3?

## Q&A

Splunk partner asking about predictions. But they *have* just hired 3 machine
learning people.

When's 1.0 due? Before 2.0 (in Q1).

Commercial support and training are the way ES make money.

LogStash and Kibana? are in ES (company or project) now. Apparently a lot of
users deploy ES as part of LogStash rather than for itself.

Replicating between clusters in different data-centres? Currently restoring is
whole-index. Coming soon is incremental restore to read-only indices. This is
high-up on the list.

For the list, see tags on the issue tracker.

What sort of ML are you going to integrate? Not sure yet, still evaluating
options (probably pick one first off); personally want an automated tuning.


