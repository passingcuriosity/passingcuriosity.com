---
title: Renaming objects in an LDAP directory
tags: howto, ldap, rename
location: Sydney, New South Wales
excerpt: 
  I'm no LDAP expert and it wasn't immediately obvious how to rename a bunch of
  objects in an LDAP directory. Here's how I did it.
---

Lightweight Directory Access Protocol is used, as the name suggests, to access
a directory of information maintained by a server. If you've never seen LDAP
before it can be a bit weird; unlike relational databases, information is
organised into a tree and objects have names like:

    cn=Thomas,ou=Users,dc=passingcuriosity,dc=com

This *distinguished name* (or DN for short) identifies an object with the
*common name* attribute `Thomas`, in the *organisational unit* `Users`, under
the *domain component* `passingcuriosity` under the domain component `com`.

Usually a directory will have a root suffix under which all other objects are
stored; the suffix is often based on a domain name (as above) or an
organisation:

    cn=Thomas,ou=Users,o=Passing Curiosity,l=Sydney,st=New South Wales,c=AU

Here we have an *organisation* in a *locality*, *state*, and *country*.

If you get this suffix wrong, if can be a bit of a pain to change. Luckily, 
