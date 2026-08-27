# Package Management Strategy

> [!NOTE]
> This document is a work in progress. It records the current direction for
> Step 7 of the dotfiles redesign; it is not yet an executable bootstrap
> specification.

## Goal

Provide a broadly consistent Fish and command-line environment across Arch
Linux, FreeBSD, macOS, RHEL-family Linux distributions, and Linux distributions
running under Windows Subsystem for Linux (WSL).

Exact system-package versions may differ between operating systems. Language
runtimes and development tools that require reproducible versions are managed
with mise instead.

## Responsibilities

- Native OS package managers install Fish and system-level prerequisites.
- GNU Stow links the managed dotfiles into `$HOME`.
- Fisher restores Fish plugins.
- mise installs and selects language runtimes and supported development tools.
- Machine-specific and secret configuration stays outside this repository.

The native package managers are:

| Platform | Package manager |
| --- | --- |
| Arch Linux | `pacman` |
| FreeBSD | `pkg` |
| macOS | Homebrew |
| RHEL family | `dnf` |
| Windows | The package manager of the WSL distribution |

The bootstrap process must not silently fall back to remote `curl | sh`
installers when a package is unavailable. Exceptions must be explicit,
documented, and reviewed separately.

## Package Tiers

### Required foundation

These packages are required to install and operate the dotfiles:

- Fish
- Git
- GNU Stow
- mise
- curl
- CA certificates

### Standard CLI environment

These tools should be installed on every supported platform when a suitable
native package is available:

- eza
- bat
- fd
- ripgrep
- zoxide
- superfile
- Neovim
- Emacs
- less

### Fish plugins

These are restored through Fisher rather than an OS package manager:

- Fisher
- Tide

### mise-managed tools

These runtimes and development tools should be declared in `mise.toml`:

- Ruby
- Rust
- Node.js
- Bun
- Go
- Lua
- Zig
- Terraform
- OpenTofu
- Other development CLIs supported reliably by mise

Rust versions are selected through mise. rustup may remain the native backend
used internally to provide the selected Rust toolchain.

### Optional and machine-specific tools

These are not required for the standard environment:

- trash-cli
- cmigemo
- safe-chain
- LM Studio
- GPU-specific tools and environment variables

The Fish configuration may integrate with an optional tool when it is present,
but Fish must still start and provide a safe fallback when it is absent.

## Arch Linux Draft

The current Arch Linux machine uses the following package mapping:

| Command | Package |
| --- | --- |
| `fish` | `fish` |
| `git` | `git` |
| `stow` | `stow` |
| `mise` | `mise` |
| `eza` | `eza` |
| `bat` | `bat` |
| `zoxide` | `zoxide` |
| `spf` | `superfile` |
| `nvim` | `neovim` |
| `emacs` | `emacs-wayland` |
| `less` | `less` |
| `fd` | `fd` |
| `rg` | `ripgrep` |
| `trash-put` | `trash-cli` (optional) |
| `cmigemo` | AUR `cmigemo-git` (optional) |

Provisional installation command:

```sh
sudo pacman -S --needed \
  fish \
  git \
  stow \
  mise \
  eza \
  bat \
  zoxide \
  superfile \
  neovim \
  emacs-wayland \
  less \
  fd \
  ripgrep \
  curl \
  ca-certificates
```

`trash-cli` is intentionally excluded from the required package list.

`cmigemo` is not in the official repositories; it is packaged in the AUR as
`cmigemo-git`. Emacs uses it for romaji-driven incremental search over Japanese
text, and the configuration disables the feature when it is absent, so it stays
optional. Note that the dictionary is installed as
`/usr/share/cmigemo/utf-8/migemo-dict`, not under `/usr/share/migemo`, which is
the path Debian-family packages use.

## Platform Research Status

| Platform | Status | Remaining work |
| --- | --- | --- |
| Arch Linux | Draft mapping recorded | Decide whether `emacs-wayland` is a platform default or a machine profile |
| FreeBSD | Incomplete | Verify every package name and superfile availability |
| macOS | Incomplete | Verify every Homebrew formula and choose the Emacs package |
| RHEL family | Incomplete | Define supported releases, EPEL policy, mise repository, and unavailable packages |
| WSL | Incomplete | Define supported distributions and reuse their Linux package mappings |

## Open Decisions

- Whether Arch should require `emacs-wayland` or use the generic `emacs`
  package.
- Which RHEL-family releases are in scope.
- Which WSL distributions are in scope.
- How to handle standard CLI tools that are absent from a native repository.
- Whether package lists should remain documentation or become idempotent
  bootstrap scripts.
- Whether package versions should be recorded for diagnostics without being
  pinned by the OS package manager.

## References

- [Fish installation options](https://fishshell.com/)
- [Arch Linux Fish package](https://archlinux.org/packages/extra/x86_64/fish/)
- [Arch Linux mise package](https://archlinux.org/packages/extra/x86_64/mise/)
- [Homebrew Fish formula](https://formulae.brew.sh/formula/fish)
- [FreeBSD eza port](https://www.freshports.org/sysutils/eza/)
- [FreeBSD mise port](https://www.freshports.org/sysutils/mise/)
