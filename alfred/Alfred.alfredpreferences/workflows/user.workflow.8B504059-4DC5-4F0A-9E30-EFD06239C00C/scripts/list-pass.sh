#!/bin/bash
# Lists pass(1) entries under ~/.password-store as Alfred Script Filter JSON
# on stdout. Each entry name is the .gpg file's path relative to the store,
# with the .gpg suffix stripped (e.g. work/vpn.gpg -> work/vpn).

store="$HOME/.password-store"
find "$store" -name '*.gpg' 2>/dev/null | sed "s|^$store/||; s|\.gpg\$||" | jq -Rs '
  split("\n") | map(select(length > 0)) | {
    items: map({uid: ., title: ., subtitle: ., arg: .})
  }
'
