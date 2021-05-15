# epubinfo

![CI](https://github.com/akirak/epubinfo/workflows/CI/badge.svg)
[![Cachix Cache](https://img.shields.io/badge/cachix-akirak-blue.svg)](https://akirak.cachix.org)

This is a small command line program which extract various information from an EPUB file. It is intended for use in scripting in Bash, Emacs Lisp, or whatever language you use.

## Features

It has the following commands:

- `metadata`, which prints metadata of the file in JSON.
- `toc`, which prints the table of contents in Org or Markdown.

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

An example output formatted with `jq`:

```
{
  "identifier": [
    "https://leanpub.com/the-devops-toolkit-catalog"
  ],
  "title": [
    "The DevOps Toolkit: Catalog, Patterns, And Blueprints"
  ],
  "language": [
    "en"
  ],
  "contributor": [],
  "creator": [
    "Viktor Farcic and Darin Pope"
  ],
  "date": "2020-06-11",
  "subject": [],
  "publisher": "leanpub.com",
  "meta": {
    "cover": [
      "cover_image"
    ],
    "dcterms:modified": [
      "2021-04-08T17:35:52Z"
    ],
    "file-as": [
      "Viktor Farcic and Darin Pope"
    ],
    "role": [
      "aut"
    ],
    "title-type": [
      "main"
    ]
  }
}
```

### Printing the table of contents in plain text

``` shell
epubinfo toc YourEbookFile.epub
```
