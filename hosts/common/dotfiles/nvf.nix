{pkgs, ... }: {
    vim = {
        theme = {
            enable = true;
            name = "tokyonight";
            style = "night";
        };
        viAlias = true;
        vimAlias = true;

        options = {
            shiftwidth = 4;
            tabstop = 4;
        };

        keymaps = [
            {
                key = "<ESC>";
                action = "<ESC>l";
                mode = ["i"];
            }
        ];

        binds.whichKey = {
            enable = true;
        };

        # startPlugins = [{
        #     pname = "auto-dark-mode";
        #     version = "1";

        #     src = pkgs.fetchFromGitHub {
        #         owner = "f-person";
        #         repo = "auto-dark-mode.nvim";
        #         rev = "02ef9553e2a1d6e861bc6955d58ce5883d28a6ad";
        #         hash = "1hwdb8ppcxbfjxrbs9s0lphq0gi7vj6zrhd6fpql77xd7s4ydrg4";
        #     };

        #     # Whether to place plugin in /start or /opt
        #     optional = false;

        #     # Plugins can have other plugins as dependencies
        #     # this is mainly used in nixpkgs
        #     # avoid it if possible
        #     dependencies = [];
        # }];

        extraPlugins = with pkgs.vimPlugins; {
            telescope-fzf-native = {
                package = telescope-fzf-native-nvim;
                setup = "require('telescope').load_extension('fzf')";
            };

            neogit = {
                package = neogit;
                setup = "require('neogit').setup()";
            };
        };

        statusline.lualine.enable = true;
        telescope.enable = true;
        autocomplete.nvim-cmp.enable = true;
        autopairs.nvim-autopairs.enable = true;

        languages = {
            enableLSP = true;
            enableTreesitter = true;
            enableFormat = true;

            nix.enable = true;
            markdown.enable = true;
            bash.enable = true;

            python.enable = true;
        };
    };
}
