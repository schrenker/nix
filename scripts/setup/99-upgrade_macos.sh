#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
if [[ "${TRACE-0}" == "1" ]] || [[ "${TRACE-0}" == true ]]; then
    set -o xtrace
fi

if [[ "${1-}" =~ ^-*h(elp)?$ ]]; then
    echo 'Usage: ./99-upgrade_macos.sh

Reactivate nix-darwin after macos upgrade.
'
    exit
fi

cd "$(dirname "$0")"

main() {
    if [[ -f "/etc/bashrc" ]]; then
        sudo mv /etc/bashrc /etc/bashrc.orig
    fi

    if [[ -f "/etc/zshrc" ]]; then
        sudo mv /etc/zshrc /etc/zshrc.orig
    fi

    if [[ -f "/etc/zprofile" ]]; then
        sudo mv /etc/zprofile /etc/zprofile.orig
    fi

    sudo /nix/var/nix/profiles/system/activate

    echo "Restart your shell and run 'sw'."
    exit 0
}

main "$@"
