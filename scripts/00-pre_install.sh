#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
if [[ "${TRACE-0}" == "1" ]] || [[ "${TRACE-0}" == true ]]; then
    set -o xtrace
fi

if [[ "${1-}" =~ ^-*h(elp)?$ ]]; then
    echo 'Usage: ./00-pre_install.sh.sh

'
    exit
fi

cd "$(dirname "$0")"

main() {
    ls /nix > /dev/null 2>&1

    STATUS=$?
    if [ $STATUS = 0 ]; then
        echo "STOP. Read the following."
        echo
        echo "If you are installing nix from fresh, then there's something wrong."
        echo "Path /nix exists on your system, so it either means, that this system already has nix installed, or there are some remains after uninstallation."
        echo "Please check before proceeding with other scripts."
        exit 1
    fi

    exit 0
}

main "$@"
