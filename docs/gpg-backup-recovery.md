# GPG Key Backup & Recovery

## Backup

The private key is backed up as printed text on paper using `paperkey`, which strips all
public key material and outputs only the secret bytes as human-readable base16 (hex).

```sh
gpg --export-secret-keys <KEY_ID> | paperkey > gpg-secret.txt
```

Print `gpg-secret.txt` and store it somewhere safe. The output is a comment block
explaining the format followed by 16 lines of hex -- keep both when printing.

Example of comment block:

```text
# Secret portions of key ...
# Base16 data extracted <DATE>
# Created with paperkey 1.6 by David Shaw
#
# File format:
# a) 1 octet:  Version of the paperkey format (currently 0).
# b) 1 octet:  OpenPGP key or subkey version (currently 4)
# c) n octets: Key fingerprint (20 octets for a version 4 key or subkey)
# d) 2 octets: 16-bit big endian length of the following secret data
# e) n octets: Secret data: a partial OpenPGP secret key or subkey packet as
#              specified in RFC 4880, starting with the string-to-key usage
#              octet and continuing until the end of the packet.
# Repeat fields b through e as needed to cover all subkeys.
#
# To recover a secret key without using the paperkey program, use the
# key fingerprint to match an existing public key packet with the
# corresponding secret data from the paper key.  Next, append this secret
# data to the public key packet.  Finally, switch the public key packet tag
# from 6 to 5 (14 to 7 for subkeys).  This will recreate the original secret
# key or secret subkey packet.  Repeat as needed for all public key or subkey
# packets in the public key.  All other packets (user IDs, signatures, etc.)
# may simply be copied from the public key.
#
# Each base16 line ends with a CRC-24 of that line.
# The entire block of data ends with a CRC-24 of the entire block of data.
```

The public key is not secret and is synced via Syncthing. Both formats are kept: the
armored `.asc` for general use and the binary `pubring.gpg` which paperkey requires
during recovery.

```sh
gpg --export --armor <KEY_ID> > ~/Sync/gpg-public.asc
gpg --export <KEY_ID> > ~/Sync/pubring.gpg
```

## Recovery

**Requirements:** `paperkey` (`brew install paperkey`), `gpg`

1. OCR or manually type the hex lines from the printed paper into a text file (e.g. `gpg-secret.txt`).

2. Reconstruct and import:
```sh
paperkey --secrets gpg-secret.txt --pubring ~/Sync/pubring.gpg | gpg --import
```

3. Verify:
```sh
gpg --list-secret-keys
```
