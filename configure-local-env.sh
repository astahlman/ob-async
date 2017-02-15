#!/bin/bash
set -e

THIS_FILE="$(cd "$(dirname "$1")"; pwd)/$(basename "$1")$(basename "$0")"
echo "Running tests using '$(which emacs)'. To change this, edit $THIS_FILE"
## export EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
