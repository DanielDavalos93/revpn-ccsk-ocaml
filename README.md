## Overview

This repository contains the OCAML implementation of theoretical framewokr apperead in [1]. 
The work is part of the [QCOMICAL](https://wdi.centralesupelec.fr/qcomical/) MSCA-SE secondments of Daniel Dávalos at
* [University of Urbino](https://www.uniurb.it/) (period ..) under the supervision of Claudio Antares Mezzina;
* University of Cagliari (period 19/7/26-19/8/26) under the supervision of G. Michele Pinna

# revpn-ccsk-ocaml

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

Clone the repository:

```bash
git clone https://github.com/DanielDavalos93/revpn-ccsk-ocaml.git
cd revpn-ccsk-ocaml
```

## Rerefences

[1] Hernán C. Melgratti, Claudio Antares Mezzina, G. Michele Pinna:
Encoding Reversible Petri Nets into CCSK. Components Operationally: Reversibility and System Engineering: Essays Dedicated to Jean-Bernard Stefani on the Occasion of His 65th Birthday 2026: 3-23
