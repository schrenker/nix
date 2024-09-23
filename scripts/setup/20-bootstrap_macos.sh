#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
if [[ "${TRACE-0}" == "1" ]] || [[ "${TRACE-0}" == true ]]; then
    set -o xtrace
fi

if [[ "${1-}" =~ ^-*h(elp)?$ ]]; then
    echo 'Usage: ./20-bootstrap_macos.sh
'
    exit
fi

cd "$(dirname "$0")"

main() {
    echo "Backing up /etc/nix/nix.conf and shells"
    if [[ -f /etc/nix/nix.conf ]]; then
        sudo mv /etc/nix/nix.conf /etc/nix/.nix-darwin.bkp.nix.conf
    fi

    if [[ -f /etc/shells ]]; then
        sudo mv /etc/shells /etc/shells.bkp
    fi

    echo "Linking certificates for the build time"
    if [[ -f /etc/ssl/certs/ca-certificates.crt ]]; then
        sudo rm /etc/ssl/certs/ca-certificates.crt
        sudo ln -s /nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt /etc/ssl/certs/ca-certificates.crt
    fi

    cd ~/.config/nix

    echo "Building flake"
    nix build .#darwinConfigurations.Macbook.system

    echo "Activate first environment"
    if [[ -f /etc/nix/nix.conf ]]; then
        sudo mv /etc/nix/nix.conf /etc/nix/nix.conf.before-nix-darwin
    fi

    if [[ -f /etc/bashrc ]]; then
        sudo /etc/bashrc /etc/bashrc.before-nix-darwin
    fi

    if [[ -f /etc/ssl/certs/ca-certificates.crt ]]; then
        sudo unlink /etc/ssl/certs/ca-certificates.crt
    fi

    ./result/sw/bin/darwin-rebuild switch --flake .#Macbook

    echo "Changing current user login shell to fish"
    echo "/etc/profiles/per-user/sebastian/bin/fish" | sudo tee -a /etc/shells
    sudo chsh -s /etc/profiles/per-user/sebastian/bin/fish sebastian

    sudo nix-env -e nix
    echo "Cleanup"
    rm -r result

    nix-collect-garbage -d

    echo "Restart your shell, and run 'sw'."
}

main "$@"
