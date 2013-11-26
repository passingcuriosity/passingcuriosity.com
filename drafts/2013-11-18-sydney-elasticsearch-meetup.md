---
title: Inaugural Sydney Elasticsearch Meetup
tags: elasticsearch, search, meetup, event
location: Sydney, New South Wales
excerpt: 
  The inaugural Sydney Elasticsearch Meetup had talks on monitoring
  Elasticsearch clusters and on the impending 1.0 release.
---

# Sharif Olorin on Monitoring Elasticsearch

Sharif is a system administrator at Anchor Systems and has been working with
Elasticsearch for about a year.

monitor everything
only alert metrics ppl care about
alert clusters
automate recovery
monitor all the thoings
stats api is your friend
store everything (just in case you need it); not RRD if you can avoid it.

## cluster-wide

individual nodes cant be tristed for cluster metrics. Eg detecting split brains.

This approach doesnt play well w/ most hostbased monitoring (e.g. nagios). Need
to define custom checks which, themeselves, work on each host.

Lots of these checks really need concurrency to be fast. Showed an example of
the split brain check in Golang.

## alerting

Loss of redundancy is not always an error.

Automated response can be appropriate. Nagios auto fix to set each node in a
cluster to read only; minimises damage and cleanup.

## Tuning

Think as statistically as possible; tune individual params, test as same time
of day, eyc.

Sometimes graphs aren't enough; sometimes you need real numbers (outliers, etc)

# Clinton Gormley on Elasticsearch 1.0

Clinton works for Elasticsearch where he 

ES around 0.4 in 2010.

## Rolling upgrades

Currently cant have clusters wuth mixed versioms. Need to bring up a new
cluster and down the old.

## Snapshot and restore (backup)

Currently a bit fiddly:

1. Disable flush
2. Copy data files
3. Enable flush

Now controlled by API. Supports shared file system storage "repositories"
accessible by all nodes (S3, Glusterfs, etc).

Restore is similarly fiddly

1. close indicies
2. fimd shards
3. replace files
4. open indices
5. wait for recobery

Also exposed through API now (though still involves closing indicies)

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
- extisting documents
- counting
- highlighht
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

Replicating between clusters in different datacentres? Currently restoring is
whole-index. Coming soon is incremental restore to read-only indeces. This is
high-up on the list.

For the list, see tags on the issue tracker.

What sort of ML are you going to itegrate? Not sure yet, still evaluating
options (probably pick one first off); personally want an automated tuning.


