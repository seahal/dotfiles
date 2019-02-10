# encoding
set -x LANG ja_JP.UTF-8

# alias
alias emacs="emacs -nw"

# 右側の日付
set -g theme_date_format "+%H:%M:%S"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/MBP/Downloads/google-cloud-sdk/path.fish.inc' ]; if type source > /dev/null; source '/Users/MBP/Downloads/google-cloud-sdk/path.fish.inc'; else; . '/Users/MBP/Downloads/google-cloud-sdk/path.fish.inc'; end; end

# rbenv
set -x PATH $HOME/.rbenv/shims $PATH
rbenv rehash >/dev/null ^&1

# homebrew
set -U fish_user_paths /usr/local/bin $fish_user_paths

# Rust Lang
set -U fish_user_paths $fish_user_paths $HOME/.cargo/bin

# nodebrew
set -U fish_user_paths $fish_user_paths $HOME/.nodebrew/current/bin/

# bobthefish
set -g theme_avoid_ambiguous_glyphs yes
set -g theme_powerline_fonts yes
set -g theme_nerd_fonts yes
set -g theme_show_exit_status yes
set -g theme_display_cmd_duration yes
set -g theme_display_git_ahead_verbose yes
set -g theme_display_git_dirty_verbose yes
set -g theme_display_git_master_branch yes
set -g theme_git_worktree_support yes
set -g theme_title_display_process yes
set -g theme_color_scheme terminal2
