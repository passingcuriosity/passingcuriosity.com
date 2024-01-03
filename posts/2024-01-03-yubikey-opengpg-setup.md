---
title: Setup Yubikey 
tags: howto, git, yubikey, gpg, ssh, github
excerpt: |
  Setup a new Yubikey with, add OpenPGP keys, and configure git and GitHub to use them.
---

# Install things

We'll install a bunch of stuff with Homebrew:

- `git` to get a newer version than what Apple ships

- `ykman` to view and tweak Yubikey configuration

- `pinentry` (to pop up a window to enter your Yubikey PIN when required)

- GnuPG 2 to take care of doing all the cryptography

```
brew install \
    git \
    ykman \
    gnupg \
    pinentry \
    pinentry-mac 
```

# Setup Yubikey

A modern Yubikey probably supports a _lot_ of different interfaces: OTP, PIV,
OpenPGP, FIDO U2F, FIDO2, OATH. Many have one or more PIN that help to prevent
unauthorised usage. If you haven't already, you should configure the various
codes for all interfaces configured on your Yubikey.

We're focussed on setting up OpenPGP so let's just take care of that.

You can use `ykman` to check the current policy for OpenPGP on your Yubikey:

```
$ ykman openpgp info
OpenPGP version:            3.4
Application version:        5.4.3
PIN tries remaining:        3
Reset code tries remaining: 0
Admin PIN tries remaining:  3
Require PIN for signature:  Once
Touch policies:
  Signature key:      Off
  Encryption key:     Off
  Authentication key: Off
  Attestation key:    Off
```

You can use `gpg --edit-card` to modify the various passwords. When started,
it'll output the current card configuration and prompt for commands. Use `admin`
to enter administration mode, then `passwd` to control the card passwords.

If you have a brand new card will have the following details:

- PIN: 123456
- Admin PIN: 12345678
- Reset Code: NONE
- Retries: 3

```
$ gpg --edit-card

...

gpg/card> admin
Admin commands are allowed

gpg/card> passwd
gpg: OpenPGP card no. D2760001240100000006196516380000 detected

1 - change PIN
2 - unblock PIN
3 - change Admin PIN
4 - set the Reset Code
Q - quit
```

You can change the number of attempts allowed for the PIN, reset code, and admin
PIN:

```
$ ykman openpgp access set-retries 10 10 10
```

# Create new keys

The general process looks something like:

```
$ gpg --gen-key
$ gpg --expert --edit-key KEY_ID
gpg> addkey
# Select whichever "set your own capabilities" option you like; it's probably a
# good idea to use the same key type as your main key.
#
# Enable Authenticate, disable the other actions (Sign and Encrypt)
#
# Set the key expiry to the same as the first key. In my case it was 3y
```

Export your key so that you can keep a backup off-line somewhere. Make sure that
it is safe, secure, and _offline_. Print it out or write it on a CD or something
and keep it with your important papers.

Then you can move the new keys to your Yubikey:

```
$ gpg --edit-key KEY_ID
# Switch to viewing private keys:
gpg> toggle
# First, move the primary key to the Yubikey. The key list will show usage of SC
# denoting a signing key, so move it to the "Signature key" slot.
gpg> keytocard
# Then select the first sub-key.
gpg> key 1
# The key list will show usage of E denoting encryption, so move it to the
# "Encryption key" slot in the Yubikey.
gpg> keytocard
# Finally, deselect the first sub-key and select the second sub-key:
gpg> key 1
gpg> key 2
# This sub-key will have usage "A", so move it to the "Authentication key" slot.
gpg> keytocard
```

At various points in this process, GnuPG will ask for the private key passphrase
(if you set one when generating the key) and the Yubikey Admin PIN.

# Setup GnuPG

```
echo "pinentry-program $(which pinentry-mac)" >> ~/.gnupg/gpg-agent.conf
killall gpg-agent
```

# Test signing

You can verify that GnuPG is configured and working correctly by signing a
sample message:


```
$ echo "Hello world" | gpg --clearsign
-----BEGIN PGP SIGNED MESSAGE-----
Hash: SHA512

Hello world
-----BEGIN PGP SIGNATURE-----
...
-----END PGP SIGNATURE-----
```

Depending on the settings for your key, you may need to enter your Yubikey PIN
(usually only the first time you've used it this session) and/or touch the key
(see the `ykman` settings above to control this).

# Configure `git`

You need to tell `git` which key to use when signing things and might like to
set a few other options which control when things are signed and when signatures
are displayed:

```
$ git config --global user.signingkey KEY_ID
```

Some settings which may help `git` to run GnuPG correctly:

```
$ git config --global gpg.program gpg
```

Some settings that control when `git` will sign things and display signatures:

```
$ git config --global log.showSignature true
$ git config --global commit.gpgSign true
$ git config --global tag.gpgSign true
```

# GitHub configuration

Export your _public_ key (note the `PUBLIC KEY` in the output below) and copy
the output.

```
$ gpg --export --armor KEY_ID

-----BEGIN PGP PUBLIC KEY BLOCK-----
....
-----END PGP PUBLIC KEY BLOCK-----
```

Go to the "SSH and GPG keys" page of your GitHub settings and click add [New GPG
key](https://github.com/settings/gpg/new). Paste the key into the text area, add
a useful comment to help identify the key, and click the save button.

When you use the GitHub.com web-site to commit changes to your code, GitHub
signs your commits with an internal key. To validate these commits, you'll need
to import and sign the key:

```
$ curl https://github.com/web-flow.gpg | gpg --import
$ gpg --lsign-key noreply@github.com
```

# References

- Yubikey PGP [Card edit](https://developers.yubico.com/PGP/Card_edit.html)

- Yubikey PGP [Importing keys](https://developers.yubico.com/PGP/Importing_keys.html)

- Yubikey PGP [Git signing](https://developers.yubico.com/PGP/Git_signing.html)

- [ykman openpgp manual](https://docs.yubico.com/software/yubikey/tools/ykman/OpenPGP_Commands.html)

- [Trusting GitHub's key](https://stackoverflow.com/a/60482908)

- `git-commit(1)` man page

- `git-tag(1)` man page

- `git-log(1)` man page
