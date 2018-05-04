---
title: Using Apache Virtual Document Roots for local development on OS X
tags: howto, dns, http, apache, local, development
location: Sydney, New South Wales
excerpt: 
  This is a quick guide to configuring Apache virtual document roots and using
  it to host development sites to your local machine.
---

Way back in 2013 I wrote a post about using
[dnsmasq for local development][dns] so that you don't have to
modify your `/etc/hosts` file. At the end of that post suggested that
I'd write another about configuring Apache along similar lines. This
is that post.

Rather than messing with your Apache configuration every time you work
on a new project, use dynamic vhosts as below. In what follows I'll
use paths which assume you're using MAMP and storing your projects in
`~/Sites/`. You should be able to adapt these instructions fairly
easily to any other setup.

Create a new file `/Applications/MAMP/conf/apache/dynamic.conf` with the
following contents:

```{.apache}
<VirtualHost *>
	ServerName  dynamic.dev
	ServerAlias *.dev

	UseCanonicalName Off

	VirtualDocumentRoot /Users/me/Sites/%-2+/htdocs
	<Directory "/Users/me/Sites">
		AllowOverride All
		Order allow,deny
		Allow from all
		Deny from none
		RewriteBase /
	</Directory>
</VirtualHost>
```

Make sure to replace both mentions of `/Users/me/Sites` with the path to your
own projects directory.

This configures Apache to serve all requests for `some.site.dev` from
`/Users/me/Sites/some.site/htdocs/`. Notice that the `.dev` has been
removed in the directory name? That's what the `%-2+` does: it
includes every host name component up to the second last. You can find
out more about this in the [`VirtualDocumentRoot`][vdocroot] documentation.

[dns]: /2013/dnsmasq-dev-osx/
[vhost_alias]: http://httpd.apache.org/docs/current/mod/mod_vhost_alias.html
[vdocroot]: http://httpd.apache.org/docs/current/mod/mod_vhost_alias.html#virtualdocumentroot
