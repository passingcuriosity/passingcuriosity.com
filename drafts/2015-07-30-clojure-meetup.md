---
title: July Clojure meetup
tags: events, meetup, functional programming, clojure, jvm, graphql
location: Sydney, New South Wales
excerpt: 
  I finally made it to a Clojure meetup. Here are some notes about an
  application which will support GraphQL.
---

Very much believes in GraphQL's impending success, so building an
application designed to be easily compatible with GraphQL once the
ecosystem of libraries, etc. develops.

GraphQL is a query language for RESTful endpoints. "Web SQL".

# Problem

GraphML is attempting to address the problem posed by the plethora of
RESTful endpoints that clients can interact with and of the clients
which will access each endpoint. Rather than manage the combinatorics,
versioning, etc. the approach is to provide a more general-purpose API
using a query language. Also addressing problems with over-/under-
available data.

# Convergence

Hans Rosling - his earliest TED talk with motion charts. Developing
motion chart interface, allow users to select dimensions; wound up
designing something a little bit like GraphQL.

Netflix's Falcor system.

# Design

Clients are trees of data (DOM, view hierachy, etc.) but databases are
not (necessarily) trees, they are graphs. Need to generate a tree view
from the graph data for the client to display; GraphQL services need
to do this: define possible roots which the system will use to
tree-ify data. Routes pick out roots?

## Service

Huey Peterson built GraphQL implementation in Clojure running on
Heroku, talks to the GitHub API. Uses manifold for getting from
API. Instaparse to parse.

## Clients

EDN; Om stores (pluggable); Datomic pull API (graph navigation).


