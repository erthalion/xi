xi
==

**xi** is a minimalistic xmpp client, inspired by [ii](http://tools.suckless.org/ii/) irc client. **xi** written in Haskell and uses the awesome [pontarius xmpp](https://github.com/pontarius/pontarius-xmpp/) library for the XMPP interaction.
Unfortunately, pontarius xmpp has an unpleasant [bug](https://github.com/pontarius/pontarius-xmpp/issues/46) in 0.4.0.1 version, so you must use this library from github directly.

Installation
============

```
# clone pontarius from github
git clone http://github.com/pontarius/pontarius-xmpp .deps/pontarius-xmpp
cabal sandbox init
cabal sandbox add-source .deps/pontarius-xmpp
cabal install --only-dependencies
cabal build
```

Usage
=====

Set the *username*, *password*, *server* in the *xi.yml*, then run
```
xi
```

Client will make a several directories like **contact_name@server.com** with **in**/**out** files in the current directory.

Links
====
[Blog post](http://erthalion.info/2014/03/25/xi/)
