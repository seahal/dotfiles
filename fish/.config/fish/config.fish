if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Paths
fish_add_path $HOME/.local/bin

# Add-ons
zoxide init fish | source

# ~/.local/bin/mise activate fish | source
mise activate fish | source

# abbrs
abbr -a y yazi
abbr -a nv nvim
abbr -a e emacs

# setting
set -U fish_greeting # unshow greeting of fish shell opeing.
set -x PAGER less #
set -x EDITOR nvim
set -x VISUAL nvim

# alias
alias ls='eza'
alias ll='eza -ahl --git'
alias lt='eza -T'
alias cat='bat --paging=never'

# Safe-chain Fish initialization script
source ~/.safe-chain/scripts/init-fish.fish

# trash-cli
function rm
    trash-put $argv
end

# tide
set -U tide_right_prompt_items status cmd_duration context jobs direnv bun node python rustc java php pulumi ruby go kubectl distrobox toolbox terraform nix_shell crystal elixir zig
set -g tide_aws_enabled false
set -g tide_azure_enabled false
set -g tide_oci_enabled false
set -g tide_gcloud_enabled false
