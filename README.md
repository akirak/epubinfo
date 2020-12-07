# epub2json

![CI](https://github.com/akirak/epub2json/workflows/CI/badge.svg)
[![Cachix Cache](https://img.shields.io/badge/cachix-akirak-blue.svg)](https://akirak.cachix.org)

This is a small program which parses the metadata of a EPUB 3 file and prints part of it in JSON. It is intended for use in scripting in Bash, Emacs Lisp, or whatever. In bash, you can use `jq` to retrieve a particular field from the output.

## Installation

You can install the program using Nix.

First install [cachix](https://github.com/cachix/cachix) and enable the binary cache from my account:

``` shell
cachix use akirak
```

Then run Nix to install the executable:

``` shell
nix-env -if .
```

You can also run it with a flake:

``` shell
nix run --no-update-lock-file github:akirak/epub2json
```

## Usage

Run `epub2json` executable with an input file:

``` shell
epub2json YourEbookFile.epub
```
