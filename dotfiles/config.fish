#!/usr/bin/env fish

function fish_title
    hostname
    echo ":"
    pwd
end

if status is-interactive
    fish_vi_key_bindings
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
    fish_default_key_bindings
end

direnv hook fish | source

# pyenv init - | source

# status --is-interactive; and source (pyenv virtualenv-init -|psub)
