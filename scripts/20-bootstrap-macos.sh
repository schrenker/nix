#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
if [[ "${TRACE-0}" == "1" ]] || [[ "${TRACE-0}" == true ]]; then
    set -o xtrace
fi

if [[ "${1-}" =~ ^-*h(elp)?$ ]]; then
    echo 'Usage: ./20-bootstrap.sh
'
    exit
fi

cd "$(dirname "$0")"

main() {
    echo "Backing up /etc/nix/nix.conf and shells"
    sudo mv /etc/nix/nix.conf /etc/nix/.nix-darwin.bkp.nix.conf
    sudo mv /etc/shells /etc/shells.bkp

    echo "Linking certificates for the build time"
    sudo rm /etc/ssl/certs/ca-certificates.crt
    sudo ln -s /nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt /etc/ssl/certs/ca-certificates.crt

    cd ~/.config/nix

    echo "Building flake"
    nix build .#darwinConfigurations.Macbook.system

    echo "Activate first environment"
    sudo mv /etc/nix/nix.conf /etc/nix/nix.conf.before-nix-darwin
    sudo unlink /etc/ssl/certs/ca-certificates.crt
    ./result/sw/bin/darwin-rebuild switch --flake .#Macbook

    echo "Changing current user login shell to fish"
    echo "/etc/profiles/per-user/sebastian/bin/fish" | sudo tee -a /etc/shells
    chsh -s /etc/profiles/per-user/sebastian/bin/fish

    echo "Activate the environment from PATH"
    darwin-rebuild switch --flake .

    nix-env -e nix
    # echo "Cleanup"
    # rm -r result
}

main "$@"
