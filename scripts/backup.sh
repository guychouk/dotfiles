#!/usr/bin/env bash

rsync -avh --delete --progress -e "ssh -p 23" /mnt/goliath/Backups/ $(pass show sbox)@$(pass show sbox).your-storagebox.de:/home/Backups/
