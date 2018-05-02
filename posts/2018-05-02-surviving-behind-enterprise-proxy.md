---
title: Surviving behind an enterprise proxy
location: Sydney, New South Wales
tags: howto, osx, enterprise, local, http, proxy, configuration
excerpt:
  This is a short guide to techniques for surviving behind an
  enterprise proxy.
---

Many large organisations require all machines in their network to use a
corporate proxy for all access to the internet. Often these proxies
force a terrible user experience on anyone who isn't using a Microsoft
web browser on Windows platform using centralised authentication. There
are some tools that can help make life less terrible for developers in
such situations.

Authenticating proxy requests
=============================

Many corporate proxies require client authentication (so they can
monitor your Facebook or whatever). Entering your password every time
some new process makes a request is awful but it's also moderately easy
to solve this problem: install a local proxy configured to add
authentication details and forward all requests to the official proxy.

[SquidMan][1] and [cntlm][2] are both quick and easy ways to get a local
proxy server up and running. SquidMan is a GUI app that configures and
runs a `squid` proxy for you while `cntlm` is a cut down proxy server
designed to forward requests with NTLM authentication to an upstream
proxy.

[1]: http://squidman.net/squidman/
[2]: http://cntlm.sourceforge.net/

Proxy settings for the shell
============================

Configuring your local proxy in *System Preferences.app* will get almost
all native Mac apps to use it but what about command line applications?
Here's a small script which will interrogate your system preferences and
translate them into environment variables for command line applications.

```.bash
#!/usr/bin/env bash

proxy () {
    p="$1"
    enable="$(scutil --proxy | grep "${p}Enable" | cut -d: -f2)"
    host="$(scutil --proxy | grep "${p}Proxy" | cut -d: -f2 | tr -d " ")"
    port="$(scutil --proxy | grep "${p}Port" | cut -d: -f2 | tr -d " ")"
    if [[ "$enable" -eq 1 ]] ; then
        echo "${host}:${port}"
    fi
}

http=$(proxy HTTP)
https=$(proxy HTTPS)
ftp=$(proxy FTP)

[ -n "$http" ] && echo export http_proxy=\"$http\"
[ -n "$https" ] && echo export https_proxy=\"$https\"
[ -n "$ftp" ] && echo export ftp_proxy=\"$ftp\"
```

You can run it from your `.bashrc` file like so (assuming it's installed
in your `~/bin` directory):

```.bash
eval $($HOME/bin/proxy)
```

If you use some other shell than `bash` you should be able to figure out
how to modify this yourself. You might also like to modify it to export
the capitalised variables (`HTTP_PROXY`, etc.) and make the values a
full URI (`http://${host}:${port}`) as appropriate.

Docker
======

If you use Docker for Mac in the default configuration, it should be
able to pull images via the proxy configured in *System
Preferences.app*. Getting processes *inside* containers to use the proxy
needs some additional configuration. If you run a local proxy as
described above (listening on port `2138`) then you can get Docker to
define the appropriate environment variables (as least [for `docker
build` and `docker run`][9]) by editing `$HOME/.docker/config.json`:

[9]: https://github.com/docker/for-mac/issues/2320#issuecomment-354887432

```.json
{
  "proxies":
  {
    "default":
    {
      "httpProxy": "http://host.docker.internal:3128",
      "httpsProxy": "http://host.docker.internal:3128",
      "noProxy": "http://host.docker.internal:3128",
      "ftpProxy": "http://host.docker.internal:3128"
    }
  }
}
```

Incidentally `host.docker.internal` is a magical hostname provided by
Docker (or maybe just Docker for Mac? Who knows!) that resolves to your
host.

SSH
===

Sometimes you just want an SSH connection but all outgoing connections
are blocked unless they go via the proxy. You can use tools like
[`corkscrew`][3] to tunnel SSH (and most other TCP protocols) through a
proxy.

[3]: https://github.com/bryanpkc/corkscrew

With the `ProxyCommand` directive in your `.ssh/config` OpenSSH can use
some other command to manage the shipping the bytes back and forth to
the server. A command like `corkscrew` can do this shipping via your
proxy server:

````
Host eg1
    Hostname host1.example.com
    ProxyCommand corkscrew $HTTP_PROXY $PROXY_PORT %h %p
````

Now `ssh eg1` will invoke `corkscrew` which will `CONNECT` through the
proxy. Some particularly obnoxious proxies will be configured to block
this, YMMV.

What about those crazy access rules
===================================

Complex environments will often have ludicrous rules requiring you
access some internal services directly, using a special purpose proxy
for third-party service A, and so on. These requirements are usually
implemented using perhaps the world's stupidest use of JavaScript:
Proxy Automatic Configuration scripts.

Alas, there's not much you can do about these. On current versions of OS
X configuring a PAC and HTTP and HTTPS proxies will not do anything
useful: it just ignores the HTTP and HTTPS proxy settings and always
uses the PAC. The best alternative I've found is to use a browser
extension like [Proxy SwitchyOmega][4] to control which requests use the
PAC and which use your local proxy server. Generally you'll only
actually want to use the policy in the PAC from your web browser anyway.

[4]: https://chrome.google.com/webstore/detail/proxy-switchyomega/padekgcemlokbadohgkifijomclgjgif?hl=en
