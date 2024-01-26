---
title: Extract a CA certificate bundle from macOS Keychain
tags: howto, certificates, python, curl, openssl, keychain
excerpt: |
  How to export CA root certificates from macOS keychain as a PEM file that can
  be used with tools that don't easily integrate with the system trust store
  (e.g. almost anything UNIX-y).
---

How to generate a PEM file containing the trusted CA certificates in your macOS
Keychain. As described in [this answer on Stack Exchange][1] you can use the
[`security`][2] tool added in Mac OS X 10.3:

```
security find-certificate -a -p /System/Library/Keychains/SystemRootCertificates.keychain > cacerts.pem

security find-certificate -a -p /Library/Keychains/System.keychain >> cacerts.pem
```

If there are missing CA certificates you need to trust, just append them to the
end of the file:

```
cat MyTlsStrippingCorporateProxyCA.pem >> cacerts.pem
```

You can store the `cacerts.pem` file somewhere convenient -- maybe somewhere
under `~/Library/` would be sensible on macOS -- and then export the many and
varied environment variables that will configure various tools to use the file:

```
export AWS_CA_BUNDLE="$HOME/Library/cacerts.pem"
export CURL_CA_BUNDLE="$HOME/Library/cacerts.pem"
export HTTPLIB2_CA_CERTS="$HOME/Library/cacerts.pem"
export REQUESTS_CA_BUNDLE="$HOME/Library/cacerts.pem"
export SSL_CERT_FILE="$HOME/Library/cacerts.pem"
export NODE_EXTRA_CA_CERTS="$HOME/Library/cacerts.pem"
```

[1]: https://stackoverflow.com/a/41853880
[2]: https://ss64.com/mac/security.html
