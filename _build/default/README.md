# revpn-ccsk-ocaml

## Overview

This repository contains the OCaml implementation of theoretical framework apperead in [1]. 
The work is part of the [QCOMICAL](https://wdi.centralesupelec.fr/qcomical/) MSCA-SE secondments of Daniel Dávalos at
* [University of Urbino](https://www.uniurb.it/) (period 19/4/26-18/6/26) under the supervision of Claudio Antares Mezzina;
* [University of Cagliari](https://www.unica.it/it) (period 18/6/26-17/7/26) under the supervision of G. Michele Pinna


## Reversible Petri Nets with Causal-Consistent Semantics (OCaml implementation)

This repository contains an OCaml implementation of a framework for modelling and executing **causally-consistent reversible concurrent systems** using **reversible Petri nets**.

The project provides a concrete implementation of reversible computations where executions can be rolled back while preserving causal dependencies between events. Unlike simple backtracking approaches, causal-consistent reversibility allows independent computations to be undone in different orders while preventing causality violations.

## Features

- Representation of Petri nets with reversible transitions
- Forward and backward execution semantics
- Causal dependency tracking between events
- Exploration of reachable states
- Simulation of reversible concurrent computations

## Motivation

Reversible computation has applications in:

- fault recovery and fault-tolerant systems
- reversible debugging
- analysis of concurrent and distributed programs
- verification of non-deterministic computations

The implementation follows the idea that every executed event records enough causal information to determine which actions can be safely reversed.

## Installation

### Installation with opam (recommended)

```bash
# Install opam
sudo apt-get install -y opam
opam init -y
eval $(opam env)

# Install dune
opam install -y dune

```

### Installation with Ubuntu/Debian

```bash
sudo apt-get update
sudo apt-get install -y ocaml ocaml-nox ocaml-dune

```

After the ocaml installation, clone the repository:

```bash
git clone https://github.com/DanielDavalos93/revpn-ccsk-ocaml.git
cd revpn-ccsk-ocaml
```

## Compile

```bash
cd revpn-ccsk-ocaml
dune build
cd _build/default/bin
./algorithm1.exe
```

## Rerefences

[1] Hernán C. Melgratti, Claudio Antares Mezzina, G. Michele Pinna:
Encoding Reversible Petri Nets into CCSK. Components Operationally: Reversibility and System Engineering: Essays Dedicated to Jean-Bernard Stefani on the Occasion of His 65th Birthday 2026: 3-23
