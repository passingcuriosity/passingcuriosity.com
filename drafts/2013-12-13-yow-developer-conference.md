---
title: Yow! Conference, Sydney 2013
location: Sydney, New South Wales
tags: event, conference, yow, software development, functional programming, security, csharp
excerpt: 
  December 12-13 saw the Yow! Australian Developer Conference finally reach
  Sydney. Here are my notes from the two days of sessions.
---

- ~40 speakers
- ~440 attendees
- three cities

YOW! LambdaJam in May was excellent and this was pretty great too. The YOW!
people seem to put on great conferences.

# Day one

## Jeff Hawkins on machine intelligence

The day kicked off with Jeff Hawkins (of Palm and Handspring fame) giving a
keynote in which he described the neurologically-inspired approach to machine
intelligence being developed by his current company ([Grok Solutions][1.1]) and
others. The basis of this approach is in building learning systems with many of
the properties of biological intelligence (universality, robustness, etc.) by
modelling them on the operation of neural structures in the [neocortex][1.2].

One of the key points was the use of representations which enable data storage
and processing in ways which are efficient and accurate *enough* for machine
intelligence. In particular, the use of *sparse distributed representations*
(SDR) is key to the model of intelligence described. Dense representations
(such as ASCII) use a very small number of bits to represent particular states
but each bit is devoid of semantic information: the state of "bit 3" in an
ASCII character conveys no useful information. An SDR uses many more bits, each
representing a particular feature in the learning domain (e.g. a property of
objects or a word in a corpus); as such, most bits in a particular SDR instance
will be 0 (hence the "sparse" in the name).

SDRs have several properties which make them useful for learning tasks: similar
objects have similar representations; they allow sub-sampling without losing
all meaning; they behave well with union/membership and other set operations
(an SDR is, in some sense, similar to a [Bloom filter][1.3]). According to Jeff:

> "All intelligent machines will be based on sparse distributed representations."

The *cortical learning algorithm* developed by Grok Systems and implemented in
the [Numenta Platform for Intelligent Computing][1.Z] open source project
(GPLv3) builds on these ideas and implements a learning system modelled on a
cortical region to learn about "normal" inputs and then predict and detect
anomalies from streaming input. Jeff described two applications in which this
software has been deployed: monitoring and detecting anomalies in monitoring
server metrics, and natural language processing.

The first example (built by Grok Systems and included in the NuPIC open source
project) is used to monitor metrics from resources in Amazon Web Services and
to detect anomalies in their behaviour. This approach can identify conditions
which traditional (and, it must be said, much, much simpler) threshold-based
approaches cannot.

The second example -- developed by [CEPT Systems][1.6] -- derives SDRs of words
from Wikipedia pages and then deploys these SDRs in particular learning
problems. This can be used to demonstrate the set-like properties of SDRs:
sdr(apple) - sdr(fruit) = sdr(computer). A CLA trained on inputs like "ANIMAL
VERB OBJECT" was able to make sensible predictions for new inputs it hadn't
seen before, including "fox" and "eat" yielding "rodent".

This was a pretty great talk and got the conference off to a great start!

[1.1]: https://groksolutions.com/
[1.2]: http://en.wikipedia.org/wiki/Neocortex
[1.3]: http://en.wikipedia.org/wiki/Bloom_filter
[1.Z]: http://numenta.org/
[1.6]: http://www.cept.at/

## Charles Nutter on language engineering for the JVM

In the second session I saw [Charles Nutter][2.1]'s talk "Beyond JVM" in which
he discussed the engineering issues which face JVM-targeting languages like
[JRuby][2.2]. Charles discussed some of the pros and cons for targeting the JVM
(many of the pros *are* also cons) and then jumped into four of the key
challenges faced by the JRuby project: startup time, native interoperability,
language performance, and the lack of flexibility in the JVM (the big ball of
C++).

Charles discussed a number of ways to improve JVM and application **startup
time**: tweaking JVM flags helps, but can be fragile in the face of different
JVMs, JVM version changes, and typically impact later performance; keeping
persistent JVM instances (using tools like [Nailgun][2.3]) can be cause
problems cleaning up resources (memory leaks, background threads, etc);
pre-loading JVMs with tools like [Drip][2.4] can improve performance while
avoiding the cleanup problems with persistent JVMs.

The problem of **native interoperability** is a complex one with a range of
solutions. The traditional approach used JNI which is horrible: you write code
for both your intention ("I want to call getpid()") *and* how to implement it.
The JNR project provide a real foreign function interface on the JVM structured
into a number of layers: jffi provides platform-specific FFI functionality,
jnr-ffi defines structures, etc. to interface with jffi, jnr-posix exposes a
range of POSIX APIs (the ones JRuby have needed so far) and jnr-constants
defines a range of constants as defined on the host platform, and jnr-enxio
implements Java NIO for arbitrary file descriptors (allowing a range of I/O
functionality which can't otherwise be expressed on JVM). JNR generates code
which is as direct as possible for each particular case, resulting in very low
overheads for each call.

One of the key motivations for JRuby is **language performance**. While the JVM
specification made mention of non-Java languages, it didn't go out of it's way
to actually support them. The relatively new `invokedynamic` bytecode allows
language implementers to customise invocation mechanisms to suit the specifics
of their language. The JVM will cache and optimise the results of dynamic
invocations as normal. This can result in plain ruby code run on JRuby being
faster than using a native extension under CRuby (redblack tree benchmark).

Finally, Charles discussed approaches that language implementors can use to
deal with the **inflexibility of the JVM internals**. The [Graal project][2.5]
allows language implementors to customise the way that their implementations
are optimised and emit the ASM/HotSpot intermediate representation appropriate
for the particular language's constructs. Truffle, a framework built on top of
Graal, allows you to implement an interpreter for your language (structured and
annotated in a particular way) and to automatically derive a JIT for it. (This
sounds a little like the second Futamura projection to me.)

[2.1]: http://blog.headius.com/
[2.2]: http://jruby.org/
[2.3]: http://www.martiansoftware.com/nailgun/
[2.4]: https://github.com/flatland/drip
[2.5]: http://openjdk.java.net/projects/graal/

This talk was very well presented and very informative. If I'd known it was
"about" JRuby I probably wouldn't have gone but I'm glad I did!

## Julien Verlaguet on Facebook's static typing for PHP

[Julien Verlaguet][3.1] is an engineer at Facebook and spoke about the work
they've done to improve on the PHP language with [HHVM][3.2] and "Hack" - a
statically typed version of PHP which was the primary subject of the talk.

Contrary to Facebook's earlier attempts at improving the deployment and runtime
story for PHP (the HipHop compiler translated PHP code into C++ which compiled
into a native binary), HHVM is a fairly traditional virtual machine with a JIT.
The [HHVM blog][3.3] has a bunch of interesting posts about the development of
the VM and the JIT both, go read it!

HHVM supports two source languages: normal PHP and Hack. Hack (the code name
might change) is a statically typed variant of PHP which is compatible with
PHP, uses the same run-time representations within the VM and was designed for
incremental adoption (a necessity when dealing with massive codebases like
Facebook.com).

The static typing for Hack requires that the programmer add type annotations to
class members, function parameters and return values and infers all other
types. The types supported include the basic types built-in to PHP, collections
and generics. It also distinguishes the types of nullable and non-nullable
values. PHP was not designed for type checking, so the type checker must make
several allowances. The most interesting is, perhaps, the delay of type
unification to call sites rather than function definitions.

The Hack type checker is implemented as a daemon which listens for file system
events on the code base and communicates with a client to "run" a check and
present errors. The errors are designed to give specific, useful feedback to
the programmer including references to each annotation which resulted in the
error ("it tells a story"). The checker is also able to output coloured
"coverage" style reports of code showing which code is checked/unchecked.

Conversion of existing PHP to Hack has happened in two ways: organic adoption
by developers as they and their teams take up Hack; and automatic conversion
using tools to analyse, refactor and monitor changes in the code base. This
includes support for "soft" conversions, which are monitored but not enforced
until they are known to be accurate.

[3.1]: https://www.facebook.com/julien.verlaguet
[3.2]: http://www.hiphop-php.com/
[3.3]: http://www.hiphop-php.com/blog/

Hack and HHVM sound like great improvements over PHP. I never got around to
trying HPHP before it went away but perhaps I'll give HHVM a go.

## Kevlin Henney deconstructed the SOLID principles

[Kevlin Henney][4.1]

I'm not really one for talks about methodologies and such, but Kevlin's talk
"the [SOLID][4.2] Design Principles Deconstructed" was entertaining and not a
little informative.

[4.1]: http://kevlin.tel/

[4.2]: http://en.wikipedia.org/wiki/SOLID_(object-oriented_design)

## Gilad Bracha on Dart and Newspeak

[Gilad Bracha][5.1] is an engineer at Google where he works on Dart. He spoke
about Dart and Newspeak.

[5.1]: http://bracha.org/

## Joe Albahari on concurrency in .NET

[Joe Albahari][6.1] spoke about concurrency in C# 5.

[6.1]: http://www.albahari.com/

## Scott Hanselman on the web platform

[Scott Hanselman][7.1] works on Azure and ASP.NET for Microsoft.

[7.1]: http://www.hanselman.com/

# Day Two

## Philip Wadler reprised the first monad tutorial

[Philip Wadler][8.1]

[8.1]: http://homepages.inf.ed.ac.uk/wadler/

## Aaron Bedra on behaviour and reputation based security controls

[Aaron Bedra][9.1]

[9.1]: http://aaronbedra.com/

## Sam Newman on microservice architecture

[Sam Newman][10.1]

[10.1]: http://blog.magpiebrain.com/

## Functional programming in industry

[Kornelis Sietsma][11.1], [Michael Neale][11.2] and [Jed Wesley-Smith][11.3]
gave a set of three talks about the adoption and use of functional programming
languages at three different companies.

[11.1]: http://korny.info/
[11.2]: http://www.michaelneale.net/
[11.3]: http://twitter.com/jedws

## Jay Fields on adopting Clojure

[Jay Fields][12.1]

[12.1]: http://jayfields.com/

## Daniel Spiewak on modules and the expression problem

[Daniel Spiewak][13.1]

[13.1]: http://www.codecommit.com/blog/

## Stewart Gleadow on mobile app and their APIs

[Stewart Gleadow][14.1]

[14.1]: http://www.stewgleadow.com/

# Sponsors and Exhibitors

Sponsors include Suncorp, DiUS, ThoughtWorks, Mashery, 
