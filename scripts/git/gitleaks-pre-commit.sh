#!/bin/bash
# Helper script to be used as a pre-commit hook.
# Enable this for this repo only, by copying this file to .git/hooks/pre-commit. Changes are local only.
# Alternatively, apply without modifying .git directory: git config --local core.hookspath ./githooks

gitleaksEnabled=$(git config --bool hooks.gitleaks)

if [ -z "$gitleaksEnabled" ] || [ "$gitleaksEnabled" = "true" ]
then
    gitleaks protect -v --staged
    exitCode=$?
    if [ $exitCode -eq 1 ]
    then
        echo "Warning: gitleaks has detected sensitive information in your changes."
        echo "To disable the gitleaks precommit hook run the following command:"
        echo ""
        echo "    git config hooks.gitleaks false"
        exit 1
    fi
else
    echo "gitleaks precommit disabled (enable with 'git config hooks.gitleaks true')"
fi
