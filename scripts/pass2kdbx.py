#!/usr/bin/env python3
"""
Generates /Users/guychouk/Sync/gv-gen.kdbx from the pass store.
Master password is read from the pass entry: personal/kdbx-master
"""

import os
import subprocess
from pathlib import Path
from pykeepass import create_database

STORE = Path.home() / ".password-store"
OUTPUT = Path.home() / "Sync" / "gv-gen.kdbx"
PASS_KEY_ENTRY = "personal/kdbx-master"
SKIP = {".git", ".DS_Store", ".gpg-id"}


def get_master_password():
    result = subprocess.run(
        ["pass", "show", PASS_KEY_ENTRY],
        capture_output=True, text=True
    )
    if result.returncode != 0:
        raise RuntimeError(f"Could not read pass entry '{PASS_KEY_ENTRY}': {result.stderr.strip()}")
    return result.stdout.splitlines()[0].strip()


def decrypt_entry(rel_path):
    entry_name = str(rel_path).removesuffix(".gpg")
    result = subprocess.run(
        ["pass", "show", entry_name],
        capture_output=True, text=True
    )
    if result.returncode != 0:
        return None
    return result.stdout


def parse_entry(content):
    lines = content.splitlines()
    password = lines[0] if lines else ""
    fields = {}
    notes_lines = []
    otp = None
    for line in lines[1:]:
        if line.startswith("otpauth://"):
            otp = line.strip()
        elif ": " in line:
            key, _, value = line.partition(": ")
            fields[key.strip()] = value.strip()
        elif line.strip():
            notes_lines.append(line)
    return password, fields, "\n".join(notes_lines), otp


def get_or_create_group(kp, group_path):
    group = kp.root_group
    for part in group_path:
        existing = kp.find_groups(name=part, group=group, first=True)
        if existing:
            group = existing
        else:
            group = kp.add_group(group, part)
    return group


def main():
    master = get_master_password()
    kp = create_database(str(OUTPUT), password=master)

    for gpg_file in sorted(STORE.rglob("*.gpg")):
        rel = gpg_file.relative_to(STORE)
        parts = rel.parts

        if any(p in SKIP for p in parts):
            continue

        group_parts = list(parts[:-1])
        entry_name = parts[-1].removesuffix(".gpg")

        content = decrypt_entry(rel)
        if content is None:
            print(f"[SKIP] Could not decrypt {rel}")
            continue

        password, fields, notes, otp = parse_entry(content)
        group = get_or_create_group(kp, group_parts)

        entry = kp.add_entry(
            group,
            title=entry_name,
            username=fields.get("UserName", ""),
            password=password,
            url=fields.get("URL", ""),
            notes=notes or fields.get("Notes", ""),
        )
        if otp:
            entry.otp = otp
        print(f"[OK] {rel}")

    kp.save()
    print(f"\nSaved to {OUTPUT}")


if __name__ == "__main__":
    main()
