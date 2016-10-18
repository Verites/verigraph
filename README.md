verigraph
=========

Software specification and verification tool based on graph rewriting

##Getting Started

Once you have cloned this repository, install `verigraph` by running:
```bash
  $ cabal install
```
Add `verigraph` to your PATH:
```bash
  $ echo "export PATH=${PATH}:~/.cabal/bin" >> ~/.bashrc
  $ source ~/.bashrc
```
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

## Using Stack

It is also possible to use `stack` instead of `cabal`:

```bash
  $ stack install
  $ echo "export PATH=${PATH}:~/.local/bin" >> ~/.bashrc
  $ source ~/.bashrc
```

## Tutorial

In depth "how to" tutorials are available for each stable version at [Releases](https://github.com/Verites/verigraph/releases).
