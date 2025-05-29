#!/bin/sh
# INFO: this script assumes you are using
#       bash or zsh

FARENC_PATH_DEBUG="target/debug"
FARENC_PATH_RELEASE="target/release"

# export debug path
export PATH="$FARENC_PATH_DEBUG:$PATH"

# export release path
export PATH="$FARENC_PATH_RELEASE:$PATH"
