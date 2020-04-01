---
title: Using ApacheDS for LDAP and Kerberos in Docker
tags: howto, docker, kerberos, ldap, kubernetes
excerpt:
  This is a quick rundown on using ApacheDS in a Docker container to provide
  an LDAP directory service and Kerberos KDC.
---

- Enable `kerberosServer`. Update the realm name and make sure the search
  baseDN will include all users and services which will be using Kerberos
  authentication.
- Enable `keyDerivationInterceptor` and `passwordHashingInterceptor`. Make sure
  they are configured to run in that order. Note that this means that changing
  a password requires you to send the password to ApacheDS in *plain text*. To
  select the hashing algorithm, change the interceptor class name to one of the
  [password interceptor implementations][1].



- Create `ou=Services` under your partition to store the details of your
  Kerberos service principals.
- Create `uid=krbtgt,ou=Services`.


[1]: http://directory.apache.org/apacheds/gen-docs/2.0.0-M18/apidocs/org/apache/directory/server/core/hash/package-tree.html