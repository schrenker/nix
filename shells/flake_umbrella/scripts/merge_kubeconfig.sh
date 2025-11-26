#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
if [[ "${TRACE-0}" == "1" ]] || [[ "${TRACE-0}" == true ]]; then
    set -o xtrace
fi

if [[ "${1-}" =~ ^-*h(elp)?$ ]]; then
    echo 'Usage: ./merge_kubeconfig.sh
    Merge kubeconfig files.
'
    exit
fi

cd "$(git rev-parse --show-toplevel)"

main() {

    which ls >/dev/null
    which grep >/dev/null
    which paste >/dev/null
    which kubectl >/dev/null

    cd ./kube

    KUBECONFIG="$(ls -1 | grep -vE '^config$' | paste -s -d ':')" kubectl config view --flatten >./config
}

main "$@"
