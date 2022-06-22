#!/usr/bin/env bash

/bin/bash -c "$(curl -L https://nixos.org/nix/install)"
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

echo "Reload shell and launch bootstrap.sh script"
