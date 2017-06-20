# Verigraph

[![Build Status](https://travis-ci.org/Verites/verigraph.svg?branch=master)](https://travis-ci.org/Verites/verigraph)
[![Coverage Status](https://coveralls.io/repos/github/Verites/verigraph/badge.svg?branch=master)](https://coveralls.io/github/Verites/verigraph?branch=master)
[![Code Climate](https://codeclimate.com/github/Verites/verigraph/badges/gpa.svg)](https://codeclimate.com/github/Verites/verigraph)
[![Issue Count](https://codeclimate.com/github/Verites/verigraph/badges/issue_count.svg)](https://codeclimate.com/github/Verites/verigraph)

<img src="./images/Verigraph.png" height="200px"/>

Software specification and verification tool based on graph rewriting.

[![DOI](https://zenodo.org/badge/22760294.svg)](https://zenodo.org/badge/latestdoi/22760294)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://github.com/Verites/verigraph/blob/master/LICENSE)

## Tutorial

In depth "how to" tutorials are available for each stable version at [Releases](https://github.com/Verites/verigraph/releases).

## Quick Start

### Installing via Stack

Once you have cloned this repository, install `verigraph` by running:

```bash
  $ stack setup # Will download and configure ghc if it is not installed yet
  $ stack install
  $ echo "export PATH=${PATH}:~/.local/bin" >> ~/.bashrc
  $ source ~/.bashrc
```

### Installing via Cabal

Once you have cloned this repository, install `verigraph` by running:

```bash
  $ cabal install
  $ echo "export PATH=${PATH}:~/.cabal/bin" >> ~/.bashrc
  $ source ~/.bashrc
```

### Usage

Run verigraph helper:

```bash
  $ verigraph --help
```

Some example grammars are provided in the `grammars` directory, try something like:

```bash
  $ verigraph analysis grammars/Pacman/pacman.ggx
```

If you use bash, you can enable autocompletion of verigraph options for the current
session by running the following command.

```bash
  $ source <(verigraph --bash-completion-script "$(which verigraph)")
```

## Modelling and Visualization

We use [AGG](http://www.user.tu-berlin.de/o.runge/agg/) to read and write the .ggx
and .cpx files with the Graph Grammars and their analysis.

## Contributing

We encourage you to contribute to Verigraph. Please check out the [Contributing guidelines](CONTRIBUTING.md) about how to proceed.

Everyone interacting in Verigraph and/or its tutorials, sub-projects' codebases and issue trackers is expected to follow the [Contributor Covenant Code of Conduct](CODE_OF_CONDCUT.md).

## License

Verigraph is released under the [Apache 2.0 License](LICENSE)
