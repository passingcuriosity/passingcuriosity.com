---
title: Using Rails and Flex
tags: bcperth09, events, barcamp
location: Perth, Western Australia
excerpt: 
  Notes from a talk on Using Rails and Flex at Barcamp Perth 2009.
---

Short talk about using Flex (the *other* way to create .swf, aimed explicitly
aimed at applications, rather than animation, games, etc). The approach is
based on using a controller to implement RESTfulness and the like. Flex has
lots of XMLness built-in, so this is pretty easy, once youíve got the
controllers generating XML in response to requests.

He began by getting rails to generate XML as he wanted it, and it was natural
to start making use of it with Flex which is very much based around XML as a
wire format.

### Interesting Rails extras

``````ruby
self.find_text_attributes = [ name, surname, nickname, email, description, ... ]
``````

Override the attributes that will be searched when `find_text` is called.

``````ruby
self.attribute_group :narrow, [id, customer_name]
``````

Comes from the REST controller. What does it do?

### Interesting Flex extras

Is talking about Flex stuff I don't know about. *Components* what're them
then? Are they like *modules*? Or *classes*?

Flex uses event bubbling, as you could expect being all JS/AS-y and all. This
seems correct.

### Dynamism

Some Silverlight programmers raising points about Visual Studio doing
autocomplete on XML objects, etc., etc. Apparently that isn't what we want
from Flex: it's supposed to be dynamic (makes sense given it's roots in
prototype-based JavaScript).
