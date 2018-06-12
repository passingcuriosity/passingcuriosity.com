---
title: Event Sourcing and CQRS Yow Night
tags: events, development
excerpt:
  Yow Night talk about event sourcing and CQRS at SafetyCulture.
---

Event sourcing, with CQRS, and DDD; with a discussion of some of the
pros and cons.

*Event sourcing* - state is a function of the stream of all historical events
in your system. Ledgers are event sourcing (accounting, banks, etc.) 500 years
old. Past tense, meaningful to "business".

*Command Query Responsibility Segregation* requires the ability to: put event;
get events, in order, from an offset; get events, in order, about some scope.
Clients shouldn't read the event store directly: instead *projectors* read the
stream of events and maintain read-optimised caches tailored for each workload.
Similarly they shouldn't write directly: instead *command handlers* interpret
the intent of commands issued by clients and emit events. It is a command
handler's responsibility to validate and process commands and produce events
(facts).

*DDD* talks about aggregates. Events happen *to* something. These somethings are
aggregates. This is the scope of the event store get operation. Clients express
intent by producing a command. Commands are validated "by" aggregates. If
accepted, events are produced.

Circular architecture decouples read and write paths so we can scale them
independently. Writes are usually fast and always consistent. Reads are always
fast and eventually consistent.

*Reactors* implement the business logic. Like projectors, they follow the event
stream. Rather than maintain a read cache they act, in some way, based on the
events: interacting with an external system, putting new events, etc. Many
reactors are stateful and maintain an internal projection. Usually (should?)
record their actions in the event store. Reactors tend toward being idempotent -
we like to be able to replay history without, e.g., resending every order email
we've ever sent.

Reactors is where much of the complexity is found.

# Pros

> "Treats the core of your business with respect"
>
> Represents events which have and will remain of fundamental interest to the
> "business".
>
> We don't know what requirements will arise later, so we don't know how to
> value the data. Event sourcing allows us to decide later.

Keep the cost of change lower - stability is necessarily dictated by stability
in the business, not accidentally as a side-effect of software implementation.

Record and interpret separately.

Ubiquitous language, shared with business stakeholders, derived from the
business-specific events.

Scalable and available in preference to consistent.

Provides obvious, clear boundaries for dividing work between teams, etc.

# Cons

Changing the way you change data is a risk.

GDPR, confidentiality.

Discoverability can be harder than with, say, a relational database. Data in an
event store is more opaque - you *have to* create projects to be able to really
make sense of the data.

Accidental and inherent complexity of systems. Event sourcing with CQRS is meant
to be a complex arrangement of simple things (cf monolith - simple arrangement
of complex things). Accidental complexity in CQRS systems can lead to a complex
arrangement of complex things.

Cognative load, paradigm shift, hiring, building teams, etc.

Propagation and processing delays. Inconsistency between projectors and
reactors.

> But eventual consistency is OK because everything is already shit.
> [paraphrased]
