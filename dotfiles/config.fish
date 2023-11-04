ulimit -n 1024

fish_vi_key_bindings
set -gx fish_key_bindings fish_vi_key_bindings

function __nixos_path_fix -d "fix PATH value"
    set -l result (string replace '$HOME' "$HOME" $__nixos_path_original)
    for elt in $PATH
        if not contains -- $elt $result
            set -a result $elt
        end
    end
    set -g PATH $result
end

__nixos_path_fix

function fish_title
    hostname
    echo ":"
    pwd
end

if [ "$system" = aarch64-darwin ]
    fish_add_path /opt/homebrew/sbin
    fish_add_path /opt/homebrew/bin
end

set -gx LESSHISTFILE -
set -gx GOPATH ~/.local/go

alias em='COLORTERM=TRUECOLOR emacs -nw'

if [ -n $INSIDE_EMACS ]
    fish_default_key_bindings
    set -gx fish_key_bindings fish_default_key_bindings
    function clear
        vterm_printf "51;Evterm-clear-scrollback"
        tput clear
    end
end

set -e -U fish_key_bindings
