#!/bin/bash

mkdir -p pack/myplugins/start

if [ ! -d "~/.local/share/nvim/js-debug" ]; then
	pushd ~/.local/share/nvim
	curl -L -O https://github.com/microsoft/vscode-js-debug/releases/download/v1.83.1/js-debug-dap-v1.83.1.tar.gz
	tar -xvzf js-debug-dap-v1.83.1.tar.gz
	rm -rf js-debug-dap-v1.83.1.tar.gz
	popd
fi

while IFS=' ' read -r repo dir; do
  [ -z "$dir" ] && dir=$(basename "$repo")
  git -C "pack/myplugins/start" clone "git@github.com:${repo}.git" "$dir"
done < packages.txt
