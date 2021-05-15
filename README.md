# epubinfo

![CI](https://github.com/akirak/epubinfo/workflows/CI/badge.svg)
[![Cachix Cache](https://img.shields.io/badge/cachix-akirak-blue.svg)](https://akirak.cachix.org)

This is a small command line program which extract various information from an EPUB file. It is intended for use in scripting in Bash, Emacs Lisp, or whatever language you use.

## Features

It has the following commands:

- `metadata`, which prints metadata of the file in JSON.

At present, EPUB 3 is only officially supported.

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
nix run --no-update-lock-file github:akirak/epubinfo
```

## Usage

### Printing the metadata in JSON

``` shell
epubinfo metadata YourEbookFile.epub
```
