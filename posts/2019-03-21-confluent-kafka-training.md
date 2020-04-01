---
title: Confluent Kafka Training
series: Learning Kafka
tags: distributed systems, kafka, operations, development
location:
  - Sydney, New South Wales
  - Melbourne, Victoria
excerpt:
  I've spent the last week or so attending Confluent Kafka Developer and Kafka
  Operations training courses.
---

Confluent is "the company" associated with Apache Kafka. They have a number of
commercial offerings including a hosted Kafka solution, extensions to the core
Apache Kafka project (some of them proprietary), training, and the like.
hosted environment

# Kafka Developer

The [Confluent Developer Skills for Building Apache Kafka][1] course gives an
introduction to developing applications with Apache Kafka. It's intended for
people who are already familiar with Java and/or Python (and the exercises
involve programming in Java and/or Python).

The early modules introduce topics like the usecases, fundamental concepts, and
architecture of Apache Kafka in sufficient detail to use Kafka effectively. The
later modules cover development topics in more details:

1. Using the Kafka command-line tools.
2. Writing data to Kafka with the `Producer` API (Java) and the REST Proxy (Python).
3. Reading data from Kafka with the `Consumer` API (Java and .Net Core in C#)
   and the REST Proxy (Python).
4. Using Apache Avro to encode data with the Consumer and Producer APIs (Java
   and C#).
5. Using Kafka Connect to ingest into and export from a Kafka cluster.
6. Writing streaming applications using the Kafka Streams API.
7. Using KSQL to transform data within a Kafka cluster.

If I were responsible for this curriculum would probably try to find some more
time to spend on the Kafka Streams content and extend those exercises to include
some stateful operations, joins, etc. Otherwise, I though that the topics
selected and the balance of time between them was pretty good.

# Kafka Operations

The [Confluent Operations for Apache Kafka][2] course describes the operation
and management of Apache Kafka clusters. It's indended for people who will need
to deploy and maintain Apache Kafka clusters and assumes some knowledge of
Linux/Unix, TCP/IP networking, the JVM, and Docker (because the training happens
to use Docker).

The first few modules covered a lot of the same ground as the developer course
-- what is Kafka for, etc. -- but it quickly started delving into more detail
about the structure and operation of Kafka itself.

1. 

[1]: https://www.confluent.io/training/confluent-developer-training/
[2]: https://www.confluent.io/training/confluent-operations-training/
