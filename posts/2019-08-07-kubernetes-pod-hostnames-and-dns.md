---
title: Kubernetes Pod hostnames and DNS
tags: kubernetes, networking, dns, hostnames, configuration, broken
abstract:
  I've wanted to get working DNS set up for various services deployed in
  Kubernetes clusters recently. This is as close as I've managed.
---

The goal:

- Deploy several Pods running instances of some clustering software.

- Each Pod knows its own fully qualified hostname without additional
  configuration (environment variables, etc.)

- The fully qualified hostname for each Pod actually resolves to an IP
  address (which is the IP address of the Pod itself).

- Each such IP address resolves the the fully qualified hostname.

- Optionally, a name exists which.

You might wonder why I want something like this? Many network services (by
which I mean: not just a HTTP REST microservice built by a startup for their
online peer-to-peer collectible card trading platform) actually care about
the identity of individual servers. There are many reasons a system could be
designed this way:

- Data is sharded and distributed across multiple servers and clients
  should connect to the server/s which host the data they wish to access.
  One example of this sort of system is Kafka.

- Servers and clients authenticate each other. Each server should have its
  own network identity and its own authentication secrets. This allows mutual
  authentication (because the client knows which server it is connecting to)
  and reduces the damage caused by an attacker compromising one server. An
  example of this sort of design is the Kerberos network authentication
  technology.

# Solution

One way to achieve these goals with Kubernetes uses headless services:

1. Create a `Service` with `spec.clusterIP: None`. The `spec.selector` should
   target the instance `Pod`s as usual.

2. Create one `Pod` for each instance of the application. Set `spec.subdomain`
   to the name of the `Service` and `spec.hostname` to a *unique value for
   each `Pod`*. You can managed the `Pod`s with a `Deployment`, but should
   *not* use replication; that would result in multiple `Pod`s with the same
   `spec.hostname`.

Create one headless service with a selector that will match all of the pods
you want. Choose the name well as it'll be in the fully qualified hostnames
for all the instances.

Create a `Deployment` for each instance, making sure that the Service selector
will match all the deployments.

Set the `spec.subdomain` field for the pod to the service's name.

Set the `spec.hostname` field for the pod to an instance identifier. Make
sure you give every pod a unique hostname otherwise they'll overwrite each
other's DNS records.

It's the existance of the `spec.hostname` field that causes the `A` records
to be created. If you ommit the field, each pod will know it's own fully
qualified hostname (based on the pod name and the fully qualified domain
name of )

# Notes

1. A headless service always results in an `A` record containing the cluster IP
   address for each selected pod.

2. 

