---
title: Logging Outgoing Mail For PHP Development
tags: php, testing, email
location: Sydney, New South Wales
excerpt: 
  A few tips for configuring PHP's email functionality during developing.
---

Configure PHP to send mail to [Mailcatcher][] and to log outgoing mail.

[Mailcatcher]: http://mailcatcher.me/

````{ini}
[mail function]
sendmail_path = /usr/bin/env catchmail
mail.log = /var/log/php/mail.log
````
