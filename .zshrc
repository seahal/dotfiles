# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# 初期設定

export LANG=en_US.UTF-8
export HISTSIZE=1000 # メモリに保存される履歴の件数
export SAVEHIST=100000 # 履歴ファイルに保存される履歴の件数
export RUBY_YJIT_ENABLE=1 # Ruby の JIT の機能

# Plug-in Settings.
## antigen
source ${HOME}/.config/antigen/antigen.zsh
antigen use oh-my-zsh
# ### bundle
antigen bundle agkozak/zsh-z
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-syntax-highlighting
### theme
antigen theme romkatv/powerlevel10k
# ## Finalize!
antigen apply

##  rbenv
export PATH="$HOME/.rbenv/bin:$PATH"; eval "$(rbenv init -)"
##  Nodebrew
export PATH=$HOME/.nodebrew/current/bin:$PATH
##  Pyenv
eval "$(pyenv init -)"
fpath=(~/.antigen/bundles/robbyrussell/oh-my-zsh/cache/ $fpath) ### 本当は antigen bundle deno でなんとかしたかったけど、うまく行かなかったのでこのやり方でやる。
##  deno


# keybind
bindkey -e # emacs 風味に


# alias
alias ls='ls -F --color=auto'


setopt hist_ignore_dups # 重複を記録しない
setopt EXTENDED_HISTORY # 開始と終了を記録
setopt inc_append_history # 履歴をインクリメンタルに追加
setopt hist_no_store # historyコマンドは履歴に登録しない
setopt hist_ignore_space # スペースで始まるコマンド行はヒストリリストから削除
setopt hist_ignore_all_dups # ヒストリに追加されるコマンド行が古いものと同じなら古いものを削除
setopt globdots # 隠しファイルを表示
setopt NO_BEEP # ビープ音を消す

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
