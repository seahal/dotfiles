if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Paths
fish_add_path $HOME/.local/bin

# Add-ons
if type -q zoxide
    zoxide init fish | source
end

if type -q mise
    mise activate fish | source
end

# abbrs
abbr --add --global y spf
abbr --add --global nv nvim
abbr --add --global e emacs

# setting
set -g fish_greeting
set -gx PAGER less
set -gx EDITOR nvim
set -gx VISUAL nvim

# alias
if type -q eza
    alias ls='eza'
    alias ll='eza -ahl --git'
    alias lt='eza -T'
end

if type -q bat
    alias cat='bat --paging=never'
end

# Safe-chain Fish initialization script
set -l safe_chain_init "$HOME/.safe-chain/scripts/init-fish.fish"

if test -f "$safe_chain_init"
    source "$safe_chain_init"
end

# Use the system rm command when trash-cli is unavailable.
function rm
    if type -q trash-put
        trash-put $argv
    else
        command rm $argv
    end
end

# tide
set -g tide_right_prompt_items status cmd_duration context jobs direnv bun node python rustc java php pulumi ruby go kubectl distrobox toolbox terraform nix_shell crystal elixir zig
set -g tide_aws_enabled false
set -g tide_azure_enabled false
set -g tide_oci_enabled false
set -g tide_gcloud_enabled false

# Machine-specific settings (not managed by this repository)
set -l custom_config "$__fish_config_dir/custom.fish"

if test -f "$custom_config"
    source "$custom_config"
end
