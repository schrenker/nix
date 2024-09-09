#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
if [[ "${TRACE-0}" == "1" ]] || [[ "${TRACE-0}" == true ]]; then
    set -o xtrace
fi

if [[ "${1-}" =~ ^-*h(elp)?$ ]]; then
    echo 'Usage: ./10-install_base.sh

'
    exit
fi

cd "$(dirname "$0")"

main() {
    git clone git@github.com:schrenker/nix.git ~/.config/nix

    if [[ $(uname) == "Linux" ]]; then
        curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
    else
        curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install macos
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        /opt/homebrew/bin/brew install git-crypt
    fi
}

main "$@"
