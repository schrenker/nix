#!/usr/bin/env fish

ulimit -n 1024

function fish_title
    hostname
    echo ":"
    pwd
end

fish_add_path /opt/homebrew/sbin
fish_add_path /opt/homebrew/bin
fish_add_path ~/.config/emacs/bin

set -gx LESSHISTFILE -
set -gx GOPATH ~/.local/go

if [ "$INSIDE_EMACS" = vterm ]
    function clear
        vterm_printf "51;Evterm-clear-scrollback"
        tput clear
    end
end

direnv hook fish | source
