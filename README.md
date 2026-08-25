# Dotfiles, Second Generation

This repository is the second generation of my personal dotfiles: a cleaner,
more modular home for the configuration that shapes my development environment.

The files are maintained with [GNU Stow](https://www.gnu.org/software/stow/).
Each top-level directory is an independent package that mirrors its destination
under `$HOME`. Stow connects the repository to the home directory with symbolic
links, while Git keeps the configuration versioned and portable.

## Packages

- `fish` — Fish shell configuration and plugin manifest

## Toolchain Policy

GNU Stow is the deployment layer for this repository. It installs each
dotfiles package under `$HOME` by creating symbolic links, without taking
responsibility for installing applications or language runtimes.

[mise](https://mise.jdx.dev/) is the standard runtime and tool-version manager.
Ruby, Rust, Node.js, and other supported development tools should be selected
and versioned through mise instead of being configured independently in the
Fish startup files. A runtime may still use its native backend internally—for
example, mise can coordinate Rust toolchains provided by rustup.

In short:

- Git versions the dotfiles.
- GNU Stow places the dotfiles under `$HOME`.
- mise manages language runtimes and development-tool versions.
- OS package managers install system-level prerequisites.

## Usage

Run Stow from the repository root:

```sh
stow --target="$HOME" --no-folding fish
```

Preview the operation before making changes:

```sh
stow --target="$HOME" --no-folding --simulate --verbose fish
```

Restow a package after changing its layout:

```sh
stow --target="$HOME" --no-folding --restow fish
```

Remove the symbolic links without deleting the files in this repository:

```sh
stow --target="$HOME" --delete fish
```

## Principles

- Keep packages small and organized by application.
- Track hand-written configuration, not generated state.
- Keep secrets, credentials, tokens, and machine-local data outside this
  repository.
