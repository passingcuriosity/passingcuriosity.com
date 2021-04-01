---
title: Reducing TLS client security requirements on OpenSSL and GnuTLS
location: Sydney, New South Wales
tags: howto, openssl, gnutls, diffie-hellman, configuration, security
excerpt: |
  This is how to configure common GnuTLS and OpenSSL clients to allow
  connections with servers using TLS keys and parameters now considered
  insecure.
---

Most operating systems do a pretty OK job of shipping libraries that have
relatively secure configurations. Unfortunately, lots of organisations --
especially large organisations -- do a terrible job of building secure
networks for them to run in. After "security" "appliances", TLS is one thing
that sticks out as regularly screwed up.

In my current organisation we must interact with a number of servers with,
by modern standards, insecure TLS configuration. In particular, Diffie-Hellman
parameters that are too short to be considered secure. OpenSSL and GnuTLS as
shipped in Ubuntu 20.04 both refuse to handshake with these services. Well
done!

Unfortunately, it's a large organisation so there is absolutely no chance of
getting these server configurations updated. So we need to reconfigure our
TLS client libraries to accept the low-security servers.

First, a disclaimer: I am not a cryptographer, security engineer, encryption
library developer, informed amateur, or even a particularly observant
bystander. You should be accepting security advice from me.

# GnuTLS

A great deal of GnuTLS operation is configured using a priority string. In
the version I'm looking at, the default priority string is

```
NORMAL:-VERS-ALL:+VERS-TLS1.3:+VERS-TLS1.2:+VERS-DTLS1.2:%PROFILE_MEDIUM
```

It's the `%PROFILE_MEDIUM` that does things like set the minimum lengths
for keys, DH primes, etc. You can find the various security profile options
in the GnuTLS manual at [Section 6.11 Selecting cryptographic key sizes][6.11].
My current project needs to support servers using Diffie-Hellman primes of
1024 bits, so I need to use `PROFILE_LOW`.

You can override the default priority string by editing (or creating)
`/etc/gnutls/config` like so:

```
[overrides]
default-priority-string = NORMAL:-VERS-ALL:+VERS-TLS1.3:+VERS-TLS1.2:+VERS-DTLS1.2:%PROFILE_LOW
```

This should then apply to any application that doesn't specify it's own
priority string.

# OpenSSL

Updating the OpenSSL configuration is a *much* more complicated proposition.
The configuration used by applications is stored in a section named by the
`openssl_conf` variable. This is *not* in a section, so it'll be at the top
of the file if it's set. In my experience, it often isn't set.

If it is set, go find the section it names, then follow `ssl_conf` to the
section containing the SSL configuration, then follow `system_default` to
the section *it* names.

Here, you can specify something like:

```
CipherString = DEFAULT:@SECLEVEL=1
```

If all that isn't already in your `openssl.cnf`, you need to create a new
section, which points to a section, which points to a section. This can all
go at the end of the file. Then you need to add a variable *not* in a section
that points to the first of those sections.

Here's a shell script that does just that:

```
#!/bin/sh
# Update the OpenSSL configuration to use lower default security level. This
# allows us to connect to TLS servers using insecure certificates issued by the
# internal CA.

set -eux

mv /etc/ssl/openssl.cnf /etc/ssl/openssl.cnf.orig

{
cat <<EOF
# Override the default OpenSSL configuration with less secure settings that
# allow communication with the many services that use insecure certificates
# issued by the internal CA.
openssl_conf = default_conf
EOF

cat /etc/ssl/openssl.cnf.orig;

cat <<EOF
# Default configuration for applications which use OpenSSL.
[ default_conf ]
ssl_conf = ssl_sect

[ ssl_sect ]
system_default = system_default_sect

[ system_default_sect ]
MinProtocol = TLSv1.2
# Be less secure when negotiating ciphers, verifying certificates, etc.
CipherString = DEFAULT:@SECLEVEL=1
EOF
} > /etc/ssl/openssl.cnf
```

It's probably a bit too lax but I use it in Docker images based on
`ubuntu:20.04` and it seems to do the trick.

[6.11]: https://www.gnutls.org/manual/html_node/Selecting-cryptographic-key-sizes.html
