#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
if [[ "${TRACE-0}" == "1" ]] || [[ "${TRACE-0}" == true ]]; then
    set -o xtrace
fi

if [[ "${1-}" =~ ^-*h(elp)?$ ]]; then
    echo 'Usage: ./10-install_base.sh

Install nix, and brew (if on macos).
'
    exit
fi

cd "$(dirname "$0")"

main() {
    if [[ ! -d "$HOME/.config/nix" ]]; then
        git clone git@github.com:schrenker/nix.git "$HOME/.config/nix"
    fi

    if [[ $(uname) == "Linux" ]]; then
        curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
    else
        curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install macos --nix-build-user-id-base 450 --nix-build-group-id 450

        if [[ ! $(which brew) ]]; then
            bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        fi

        if [[ ! $(which git-crypt) ]]; then
            /opt/homebrew/bin/brew install git-crypt
        fi

    fi
}

main "$@"
