---
title: PhD Thesis
thesis: True
---

### A Declarative Debugger for Haskell

Author: Bernard Pope

[Thesis](/docs/BerniePope.PhD.Thesis.pdf)

Topics: Debugging, Functional Programming, Program Transformation

#### Award

[Best PhD Thesis](http://core.edu.au/index.php/page/award-rec) from the [The Computing Research and Education Association of Australasia (CORE)](http://core.edu.au).

#### Abstract

This thesis is about the design and implementation of a debugging tool which helps Haskell programmers understand why their programs do not work as intended. The traditional debugging technique of examining the program execution step-by-step, popular with imperative languages, is less suitable for Haskell because its unorthodox evaluation strategy is difficult to relate to the structure of the original program source code. We build a debugger which focuses on the high-level logical meaning of a program rather than its evaluation order. This style of debugging is called declarative debugging, and it originated in logic programming languages. At the heart of the debugger is a tree which records information about the evaluation of the program in a manner which is easy to relate to the structure of the program. Links between nodes in the tree reflect logical relationships between entities in the source code. An error diagnosis algorithm is applied to the tree in a top-down fashion, searching for causes of bugs. The search is guided by an oracle, who knows how each part of the program should behave. The oracle is normally a human - typically the person who wrote the program - however, much of its behaviour can be encoded in software.

