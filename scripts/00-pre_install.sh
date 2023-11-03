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
