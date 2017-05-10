# Verigraph

[![Build Status](https://travis-ci.org/Verites/verigraph.svg?branch=master)](https://travis-ci.org/Verites/verigraph)
[![Coverage Status](https://coveralls.io/repos/github/Verites/verigraph/badge.svg?branch=master)](https://coveralls.io/github/Verites/verigraph?branch=master)

<img src="./images/Verigraph.png" height="200px"/>

Software specification and verification tool based on graph rewriting.

[![DOI](https://zenodo.org/badge/22760294.svg)](https://zenodo.org/badge/latestdoi/22760294)


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

## Modeling and Visualization

We use [AGG](http://www.user.tu-berlin.de/o.runge/agg/) to read and write the .ggx and .cpx files with the Graph Grammars and their analysis
