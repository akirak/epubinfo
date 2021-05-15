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

### Printing the table of contents in plain text

Print the table of contents in Markdown:

``` shell
epubinfo toc --markdown YourEbookFile.epub
```

Print the table of contents in Emacs Org mode:

``` shell
epubinfo toc --org YourEbookFile.epub
```

Both formats support `--checkbox` flag for adding checkboxes
and `--depth` for specifying a maximum depth:

``` shell
epubinfo toc --org --depth=1 --checkbox YourEbookFile.epub
```

An example output:

```
- [ ] Introduction
- [ ] Infrastructure as Code (IaC)
- [ ] Creating And Managing Google Kubernetes Engine (GKE) Clusters With Terraform
- [ ] Creating And Managing AWS Elastic Kubernetes Service (EKS) Clusters With Terraform
- [ ] Creating And Managing Azure Kubernetes Service (AKS) Clusters With Terraform
- [ ] There Is More About Infrastructure as Code (IaC)
- [ ] Packaging, Deploying, And Managing Applications
- [ ] Using Helm As A Package Manager For Kubernetes
- [ ] There Is More About Packaging, Deploying, And Managing Applications
- [ ] Setting Up A Local Development Environment
- [ ] Exploring Serverless Computing
- [ ] Using Managed Functions As A Service (FaaS)
- [ ] Using Managed Containers As A Service (CaaS)
- [ ] Using Self-Managed Containers As A Service (CaaS)
- [ ] There Is More About Serverless
- [ ] Using Centralized Logging
- [ ] Deploying Applications Using GitOps Principles
- [ ] Applying GitOps Principles Using Argo CD
- [ ] There Is More About GitOps
- [ ] Applying Progressive Delivery
- [ ] Using Argo Rollouts To Deploy Applications
- [ ] This Is NOT The End
```
