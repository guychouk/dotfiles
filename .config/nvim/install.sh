#!/bin/bash

mkdir -p pack/myplugins/start && cat packages.txt | parallel 'git -C pack/myplugins/start clone git@github.com:{}.git'
