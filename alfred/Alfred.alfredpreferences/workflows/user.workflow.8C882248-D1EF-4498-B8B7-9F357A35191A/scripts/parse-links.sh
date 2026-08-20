#!/bin/bash

# Runs mdlinks(1) on the given markdown file and emits the links as Alfred
# Script Filter JSON on stdout. Usage: parse-links.sh <markdown-file>
#
# Lines are tab-delimited (title<TAB>url) out of mdlinks, batched through a
# single jq call.

mdlinks "$1" 2>/dev/null | jq -Rs '
  split("\n") | map(select(length > 0)) | {
    items: map(split("\t") | {uid: .[1], title: .[0], subtitle: .[1], arg: .[1]})
  }
'
