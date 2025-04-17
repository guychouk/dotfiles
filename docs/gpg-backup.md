## Environment
  * Red Hat Enterprise Linux
  * GnuPG (gpg)


## Issue
  * We need to migrate GPG keys from a user on AIX to a user on RHEL. How?
  * How do I export my gpg keys for backup?
  * I need to make all of the gpg keyrings from one user available to another user on the same system. Can I just copy the `~/.gnupg` directory?


## Resolution
  1. As the original user, use the following command to export all public keys to a base64-encoded text file:
Raw
```
gpg -a --export >mypubkeys.asc

```

Use the following command to export all encrypted private keys (which will also include corresponding public keys) to a text file:
Raw
```
gpg -a --export-secret-keys >myprivatekeys.asc

```

Optionally export gpg's trustdb to a text file:
Raw
```
gpg --export-ownertrust >otrust.txt

```

  2. Transfer those files to a place that the new user can read, keeping in mind that it's bad practice to share private keys (e.g., via email or in a world-readable directory like `/tmp`), despite the fact that they are encrypted and require the passphrase to be used
  3. As the new user, execute `gpg --import` commands against the two `asc` files and then check for the new keys with `gpg -k` and `gpg -K`, e.g.:
Raw
```
gpg --import myprivatekeys.asc
gpg --import mypubkeys.asc
gpg -K
gpg -k

```

Optionally import the trustdb file as well:
Raw
```
gpg --import-ownertrust otrust.txt

```

  4. As the new user, test encryption and decryption with `gpg -er USERID` and `gpg -d` commands Keep in mind that decryption and signing will likely fail unless the user running `gpg` owns the terminal it is running on (Translation: don't `su` over to the new user; login directly via ssh or console)
