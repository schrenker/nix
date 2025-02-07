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
