---
title: Ledger accounting with GnuPG
tags: howto, accounting, ledger, gnupg
excerpt: |
  Transparently encrypt your Ledger CLI plain-text accounting journals with
  GnuPG.
---

[Ledger][1] has [support for GnuPG-encrypted journal files][2] but it's not
really documented anywhere that I've been able to find. To check that it's
enabled in your build, consult the version message:

```
$ ledger --version
Ledger 3.3.2-20230330, the command-line accounting tool
with support for gpg encrypted journals and without Python support

Copyright (c) 2003-2023, John Wiegley.  All rights reserved.

This program is made available under the terms of the BSD Public License.
See LICENSE file included with the distribution for details and disclaimer.
```

and check for "with support for gpg encrypted journals".

If it's present, then you can _just_ encrypt your journal files and
`ledger` will transparently the data as it reads. Doing this is a simple
matter of encrypting the files with your own key. If you only use
`gpg --encrypt` to encrypt files for yourself (and not to send to other
people) then the easiest way might be to configure GnuPG to encrypt with
your own key by default:

```
$ echo default-recipient-self >> ~/.gnupg/gpg.conf
```

Now you can just create some encrypted journal files:

```
$ cat 2024.journal | gpg --encrypt --armor > 2024.journal.gpg
$ ledger -f 2024.journal.gpg bal
        AUD 2,187.50  Assets
        AUD 2,150.00    Bank
           AUD 37.50    Cash
           AUD 12.50  Expenses:Food:Dining
       AUD -2,200.00  Income:Gifts
--------------------
                   0
```

Ledger handles encryption transparently at the file-access level, so you
can split up your configuration and journal postings into different
files and make each encrypted or unencrypted as you like. Personally, I
like to have a single plain-text file that defines my chart of accounts
and then includes encrypted journal files containing the actual posting.

[1]: https://ledger-cli.org/
[2]: https://github.com/ledger/ledger/pull/1949
