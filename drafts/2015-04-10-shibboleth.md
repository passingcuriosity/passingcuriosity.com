---
title: Single sign-on with Shibboleth
tags: howto, authentication, saml, shibboleth
location: Sydney, New South Wales
excerpt: 
  I've been preparing to deploy a single sign-on authentication system using
  Shibboleth at work lately. Here's a few things I've learned along the way.
---

# Planning

Planning your Shibboleth deployment is very important, even if it's just for
testing purposes! Here are some things to determine before you start:

- Assign an IP address to your IdP server. Resist the urge to locate an SP and
an IdP on the same IP address (this includes using docker containers unless the
ports are mapped to multiple, different, IP addresses).

- Determine the hostname for your IdP. This should probably be something like
`login.example.com` or `idp.example.org`.

- In the configuration I describe, the IdP uses port 443 *and* port 8443. If
you don't want to use these ports, decide which ones you will use now.

- All of the HTTP traffic involved in your Shibboleth deployment ought to be
over HTTPS; if this is a testing deployment you probably want self-signed
certificates for HTTPS, but a production deployment will want CA-signed
certificates.

- Shibboleth pretty much expects to consult an LDAP directory to authenticate
users. It *can* uses other sources, but I didn't have much luck getting that to
work. If you already have an LDAP directory, make sure you know how to
authenticate against it (get the LDAP server details, any bind credentials you
need, etc.)

- Each party in a Shibboleth deployment has an `entityID`, a URL used as
a unique identifier. I suggest sticking to a rule like "use the URL for
a party's metadata as its `entityID`".

