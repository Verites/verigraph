### Special release for performance comparison between Verigraph and [AGG](http://www.user.tu-berlin.de/o.runge/AGG/) as reported on a paper to appear in the book in memory of Hartmut Ehrig.

In order to run the experiments:

1. Install Verigraph as described below.
2. Download the AGG Tool [here](http://www.user.tu-berlin.de/o.runge/agg/down_V21_java8/index.html) and extract the files to a directory of your choice.
3. Edit the `benchs/agg-cli` file on Verigraph directory to provide the AGG path.
4. On the root directory of Verigraph, execute:

```bash
  $ ./benchs/run-critical-benchs-verigraph # Will measure verigraph execution time.
  $ ./benchs/run-critical-benchs-agg # Will measure agg execution time.
```
The tests results will be saved on files named `./benchs/verigraph-results.csv` and `./benchs/agg-results.csv`. Also, the critical pairs analysis output for each execution will be saved as `.cpx` and `.log` files in the directories `./benchs/outputs/verigraph` and `./benchs/outputs/agg`.


# Verigraph

[![Build Status](https://travis-ci.org/Verites/verigraph.svg?branch=master)](https://travis-ci.org/Verites/verigraph)
[![Coverage Status](https://coveralls.io/repos/github/Verites/verigraph/badge.svg?branch=master)](https://coveralls.io/github/Verites/verigraph?branch=master)
[![Code Climate](https://codeclimate.com/github/Verites/verigraph/badges/gpa.svg)](https://codeclimate.com/github/Verites/verigraph)
[![Issue Count](https://codeclimate.com/github/Verites/verigraph/badges/issue_count.svg)](https://codeclimate.com/github/Verites/verigraph)

<img src="./images/Verigraph.png" height="200px"/>

Software specification and verification tool based on graph rewriting.

[![Version](https://img.shields.io/github/release/Verites/verigraph.svg)](https://github.com/Verites/verigraph/releases/latest)
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

Run Verigraph helper:

```bash
  $ verigraph --help
```

Some example grammars are provided in the `grammars` directory, try something like:

```bash
  $ verigraph analysis grammars/Pacman/pacman.ggx
```

If you use bash, you can enable autocompletion of Verigraph options for the current
session by running the following command.

```bash
  $ source <(verigraph --bash-completion-script "$(which verigraph)")
```

## Modelling and Visualization

We use [AGG](http://www.user.tu-berlin.de/o.runge/agg/) to read and write the .ggx
and .cpx files with the Graph Grammars and their analysis.

## Contributing

We encourage you to contribute to Verigraph. Please check out the [Contributing guidelines](CONTRIBUTING.md) about how to proceed.

Everyone interacting in Verigraph and/or its tutorials, sub-projects' codebases and issue trackers is expected to follow the [Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md).

## License

Verigraph is released under the [Apache 2.0 License](LICENSE)
