---
title: Work allocation in Kafka Streams
series: Learning Kafka
tags: distributed systems, kafka, streaming, data
location: Sydney, New South Wales
excerpt:
  Here are some high-level notes about work allocation in a Kafka Streams
  application.
---

This is mostly an exercise in writing things to help remembering them myself.
You're better off referring to [Confluent's Kafka Streams documentation][4]
or [this blog post by Andy Bryant][3].

Kafka
=====

[Apache Kafka][1] is a "distributed streaming platform". *Messages* with keys
and values are written to *topics* ("queues", if that helps to think about
them). Each topic is divided (when it's created) into a number of *partitions*.
It's topic partitions are the unit of work in a Kafka cluster: at any given
time a single cluster node is responsible for processing the messages for a
given topic.

Applications which read from a Kafka topic can also be distributed - each
partition can be consumed ("read") by a different worker. The collection of
workers cooperating to process a topic form a *consumer group*. Kafka's
consumer group API helps to assign the work (i.e. the partitions) to the
available workers.

Kafka Streams
=============

[Kafka Streams][2] is a library for building streaming data processing
applications on top of Kafka. Streams applications are just normal Java
programs which can be deployed, monitored, and managed just like any other
Java program: as many instances as you start will self-organise and cooperate
to share the available work between them. This makes scaling Streams
applications very straightforward: just start or kill some instances (assuming
there are work units that can be re-/allocated).

A Kafka Streams application is described by a *topology* -- essentially a
directed acyclic graph with nodes representing each source, sink, and
processing step. Each topology can be split into *subtopologies* with nodes
which interact only with other nodes in the same subtopology. Because the
nodes in a subtopology only interact with each other, the subtopologies can
be executed in parallel without any coordination required. The collection
of subtopologies together with the collection of partitions in the input
topics for each subtopology will define the collection of *stream tasks*
that can be distributed across the workers in the Streams applications.

The first phase in executing a topology analyses it and the Kafka cluster
and determines the units of work that must be scheduled:

1. Partition the topology into subtopologies.

2. For each subtopology, check that the input topics have the same key
   configuration and the same number of partitions. This ensures that
   corresponding records from each of the input topics will be processed
   by the same stream task, allowing them to be joined, etc.

3. For each subtopology, generate one stream task to read from each set of
   corresponding partitions in the input topics. If subtopology 1 reads
   from topics A, B, and C and they are configured with 3 partitions then
   this will result in three stream tasks "1_0", "1_1", and "1_2".

Note that the collection of stream tasks generated from a topology is static:
both the graph in a topology and the number of partitions in a Kafka topic
are fixed at creation. The next phase allocates the stream tasks to be executed
by application instances.

4. Each instance executes a number of stream threads determined by its
   configuration. Each stream thread is a more or less independent worker
   able to process one or more stream tasks.

5. Each stream thread will connect to the Kafka cluster using the consumer
   group API. The Kafka cluster and the Streams application instances will
   cooperate to allocate the available work to the available workers. From
   the application's perspective this means allocating stream tasks to stream
   threads and from the Kafka cluster's perspective this is topic partitions
   to consumers (and it just happens to be the case that we'll co-allocate
   partitions of certain topics to the same workers).

With all this done, the Kafka Streams application is able to start processing
messages.

[1]: http://kafka.apache.org/
[2]: http://kafka.apache.org/documentation/streams/
[3]: https://medium.com/@andy.bryant/kafka-streams-work-allocation-4f31c24753cc
[4]: https://docs.confluent.io/current/streams/introduction.html
