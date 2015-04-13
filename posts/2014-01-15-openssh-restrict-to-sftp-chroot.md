---
adverts: on
title: Restricted SFTP-only access to a single directory using OpenSSH
tags: howto, ssh, sftp, chroot, linux
location: Sydney, New South Wales
excerpt: 
  This is a short guide to providing users with restricted SFTP access to a
  single directory using built-in OpenSSH functionality.
---

Most UNIX-like operating systems include the [OpenSSH][1] project's SSH client
and server software. It's relatively straightforward to configure the OpenSSH
server for a range of usecases. In this post we'll do the following:

[1]: http://openssh.org/

- Create a system group `exchangefiles`.

- Create a `/home/exchangefiles/` directory and `files/` directory within it.

- Allow users in the `exchangefiles` group to connect to the server using SFTP
  (but not SSH).

- Lock users in the `exchangefiles` group into the `/home/exchangefiles/`
  directory using a chroot.

- Restrict some other options for users in the `exchangefiles` group.

So we'll allow these users to connect to the SSH server and use SFTP to access
a specific directory, *and nothing else*.

# Preparations

First, lets create the new group:

````{.bash}
sudo addgroup exchangefiles
````

Then create the new directories:

````{.bash}
# Create the chroot directory
sudo mkdir /home/exchangefiles/
sudo chmod g+rx /home/exchangefiles/

# Create the group-writable directory
sudo mkdir -p /home/exchangefiles/files/
sudo chmod g+rwx /home/exchangefiles/files/

# Give them both to the new group.
sudo chgrp -R exchangefiles /home/exchangefiles/
````

# SSHD configuration

The OpenSSH server configuration is typically called something like
`/etc/ssh/sshd_config`. Find this file and open it in an editor; we're going to
add a section to the **end** of the file using the `Match` directive which
applies to users in our group:

````
Match Group exchangefiles
````

After that we specify the configuration directives which apply the matched
connections:

````
# Force the connection to use the built-in SFTP support.
ForceCommand internal-sftp
# Chroot the connection into the specified directory.
ChrootDirectory /home/exchangefiles
````

Let's lock down some of the additional capabilities of the OpenSSH server so
these people can't, e.g., forward connections through the server and into our
private network:

````
# Disable network tunneling
PermitTunnel no
# Disable authentication agent forwarding.
AllowAgentForwarding no
# Disable TCP connection forwarding.
AllowTcpForwarding no
# Disable X11 remote desktop forwarding.
X11Forwarding no
````

So the whole block looks like this:

````
Match Group exchangefiles
  # Force the connection to use SFTP and chroot to the required directory.
  ForceCommand internal-sftp
  ChrootDirectory /home/exchangefiles
  # Disable tunneling, authentication agent, TCP and X11 forwarding.
  PermitTunnel no
  AllowAgentForwarding no
  AllowTcpForwarding no
  X11Forwarding no
````

# Testing

To apply the configuration change, just restart your SSH server, create a user
and try to access the site.

````{.bash}
# On the server:
sudo adduser --ingroup exchangefiles testfiles
sudo service ssh restart

# On a test machine:
sftp testfiles@server.example.com
ssh testfiles@server.example.com
````

Connecting with `sftp` should result in a connection, but `ssh` should return
an error message:

> This service allows sftp connections only.
>
> Connection to server.example.com closed.

When connected you should be able to list, upload and delete files under
`/files/`.

# TODO

If you're doing something like this, you probably want the users to be able to
access each other's files -- otherwise why bother sharing a space? To do this
you'll need to set the umask so that files are created with the correct
permissions.

The `internal-sftp` command has a `-u` option which might help, but I haven't
tested this.

# Updates

- 2015-01-08: Tweaked group write permissions on `/home/exchangefiles` after
feedback from "Nik". Thanks Nik!
