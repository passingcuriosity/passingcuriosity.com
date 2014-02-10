---
title: Puppet Camp Sydney 2014
location: Sydney, New South Wales
tags: event, puppet, devops, cloud
excerpt: 
  I attended Puppetcamp Sydney yesterday. This is a quick round-up of the
  sessions.
---

[Puppet Camps][1] are regular, regional events for the Puppet community and
this is the second or third time I've attended one. They can feel a *tiny* bit
vendor-y (this should be unsurprising) but the quality of the talks and the
attendees is pretty good, in my experience.

[1]: http://puppetlabs.com/community/puppet-camp

**Nigel Kersten**'s keynote talk was aimed at a pretty broad audience (a bit of
Puppet, what's driving uptake, etc.) but also described some of the new
features in components included in the next release (IIRC) of Puppet
Enterprise. I was particularly interested to learn about [policy based
auto-signing][2] and [trusted node data][3] in Puppet 3.4+, [external facts][4]
in Factor 1.7+, more readable ouput from Hiera 1.3+, and the news that
Puppet Labs will be supporting some of their modules from the [forge][5].

[2]: http://docs.puppetlabs.com/puppet/latest/reference/ssl_autosign.html#policy-based-autosigning
[3]: http://docs.puppetlabs.com/puppet/latest/reference/lang_variables.html#trusted-node-data
[4]: http://docs.puppetlabs.com/guides/custom_facts.html#external-facts
[5]: http://forge.puppetlabs.com/

**Peter Leschev** from Atlassian described the process of introducing and
developing "infrastructure as code" in the Atlassian build engineering team. He
describe their introduction of a number of tools and measures and the impact on
confidence in infrastructure changes being made. It was interesting to see the
journey of adding code reviews, Puppet, Vagrant-based development (with
Veewee), behaviour based testing (with Cucumber), continuous integration
(Bamboo and Vagrant), profiling (Puppet's `--evaltrace` flag), automated
deployment (to staging) and notification (in HipChat). Later on I wished I'd
asked if the graphs of confidence in his slides were from measurements, or for
illustrative purposes only.

**Lindsay Holmwood** from Bulletproof described the [Flapjack][7] monitoring
system -- which seems pretty cool -- and how you'll be able to configure it
with Puppet (when he releases the Puppet module). The architecture of Flapjack
looked pretty interesting and I plan to have a play with it this weekend.

[7]: http://flapjack.io/

**Rene Medellin** spoke about NAB's move to push some of their workloads into
"the cloud" (AWS). They used Puppet as part of their SEO machine image building
process *and* in deployment as one of their monitoring and compliance tools.
Lots of Jenkins and automated building of AMIs and CloudFoundry templates and
such.

**Aaron Hicks** from Landcare Research NZ spoke about the way he uses Puppet to
formalise the configuration of the many precious snowflake machines he deals
with in a scientific research environment.

**James Dymond** and **John Painter** from Sourced Group described a series of
"Puppet in the AWS cloud" architectures they'd developed for clients in their
consulting engagements. Most interesting was their fourth (I think) solution,
where they implemented a "gateway" between AWS autoscaling notifications and
Puppet, allowing the master to sign certificates, delete node reports, etc. as
the AWS autoscaling system adds and removes nodes.

**Matt Moor** from Atlassian 

The last talk was by **Chris Barker** from Puppet Labs who gave a product
demonstration of Puppet Enterprise. I'd already used most of the features
demoed but some of the newer stuff -- especially the [event inspector][6] --
looked pretty cool.

[6]: http://puppetlabs.com/presentations/introducing-puppet-enterprises-event-inspector
