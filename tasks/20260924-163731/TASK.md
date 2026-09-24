# :CsvDropCol N, a vim command to drop a CSV column

- STATUS: OPEN
- PRIORITY: 95
- TAGS: vim

Idea from 2026-08-06, not pressing: wrap `:%!cut -d, -f1,2,4-` (keep-list, since
BSD cut has no `--complement`) as `:CsvDropCol 3`. `cut` breaks on quoted fields
with embedded commas, so an RFC 4180 file needs a csv-aware filter (Python's csv
module) behind the command instead.
