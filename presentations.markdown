---
title: Presentations
presentations: True
---

## Clinical Genomics: a Computational Perspective 

[linux.conf.au](https://linux.conf.au/), Geelong, Monday 1 February 2016

- [Slides](/docs/clinical_genomics_lca_2016.pdf)
- [Video](https://www.youtube.com/watch?v=tGTVB-dqji4&feature=youtu.be)

#### Abstract

Low cost highly accurate DNA sequencing is quickly becoming an option for clinical diagnosis and treatment. In this
talk I discuss the computational aspects of Clinical Genomics, based on recent experiences with the 
[Melbourne Genomics Health Alliance](http://www.melbournegenomics.org.au/). This was an invited talk for the [Open Source and Bioinformatics](http://afrubin.github.io/miniconf/) miniconf, as part of the Linux Conference in Geelong 2016.

****

## Another look at declarative debugging for Haskell 

VASET group, The University of Melbourne, Friday 30 October 2015.

[Slides](/docs/dd_vaset.pdf)

#### Abstract

Despite rising notoriety, Haskell still lacks effective debugging support. A breakpoint debugger exists, but, as expected, it is difficult to use in the presence of lazy evaluation and higher-order functions. We have previously considered declarative debugging as a more suitable approach for Haskell, but a number of technical challenges remain unsolved. In this talk I will revisit the main ideas behind declarative debugging and outline a few interesting areas of potential research in the context of non-strict purely functional languages, namely:

 - Generalising the structure of the debugging tree.
 - Interleaving debugging and program execution to reduce memory consumption.

****

## Lindenmayer Systems 

Foundations of Computing Advanced Lecture, The University of Melbourne, Friday 22 August 2014.

[Slides](/docs/lsystem.pdf)

#### Abstract

These slides introduce the idea of turtle graphics in Python and show how they can be used to implement
Lindenmayer Systems (term rewriting languages with interesting interpretations as graphics).

****

## Ray tracing in Python

Foundations of Computing Advanced Lecture, The University of Melbourne, Friday 5 September 2014.

[Slides](/docs/ray_tracing.pdf)

#### Abstract

These slides explain the central ideas behind the recursive ray-tracing algorithm and demonstrate how to implement 
it in Python. A basic Phong Illumination model is used in the slides and program.

[Code](https://github.com/bjpop/pyray)

****

## Computer games in Python 

Foundations of Computing Advanced Lecture, The University of Melbourne, Friday 19 September 2014.

[Slides](/docs/games_in_python.pdf)

#### Abstract

These slides show how to implement arcade games in Python using the [PyGame](www.pygame.org/) library.
To demonstrate the main ideas of game programming in Python I wrote a simple asteroids game which
is available in the repository linked below.

[Code](https://github.com/bjpop/asteroids)

****

## How Python works 

Foundations of Computing Advanced Lecture, The University of Melbourne, Friday 10 October 2014.

[Slides](/docs/how_python_works.pdf)

#### Abstract

An overview of how Python works (CPython specifically). Covers syntax analysis (lexing and parsing), compilation to
bytecode and program exeuction. The slides conclude with an example bytecode execution of a simple Python program. 

****


## Implementing Python in Haskell, twice

The Melbourne Python Users Group, Monday 7th July 2014.

[Slides](/docs/MPUG_python_haskell.pdf)

#### Abstract

Over the past couple of years I've developed an unusual hobby: implementing Python in Haskell.

The first iteration resulted in [berp](https://github.com/bjpop/berp), a Python-to-Haskell translator. The second iteration resulted in [blip](https://github.com/bjpop/blip), which compiles Python to bytecode, and is compatible with the standard Python implementation CPython.

Note: I've given a similar talk to the Haskell User's Group (see below). In this version of the talk the focus is oriented to a Python audience.

****

## Functional graphics in Scala

The Melbourne Scala Users Group, Monday 26th May 2014.

[Slides](/docs/fungraph_scala.pdf)

#### Abstract

Computer graphics are typically represented using two-dimensional arrays of pixels. In this talk I will demonstrate an alternative, and perhaps surprising, representation based on functions:

```
   type Image[T] = (Double, Double) => T
```

We assume that images are defined (infinitely) over the two-dimensional real coordinate space, which avoids the need to worry about boundary conditions or discretisation.

I will also show how this idea can be extended to animations, by making them functions over time:

```
   type Animation[T] = Double => Image[T]
```

In addition to showing some pretty pictures, I demonstrate that higher-order functions are a powerful tool for program abstraction.

All the code for my talk is available on github: [https://github.com/bjpop/scala-fungraph](https://github.com/bjpop/scala-fungraph)

****

## Implementing Python in Haskell, twice

The Melbourne Haskell Users Group, Thursday 24th April 2014.

[Slides](/docs/ImplementPythonInHaskell.pdf)

#### Abstract

Over the past couple of years I've developed an unusual hobby: implementing Python in Haskell.

The first iteration resulted in [berp](https://github.com/bjpop/berp), a Python-to-Haskell translator. The second iteration resulted in [blip](https://github.com/bjpop/blip), which compiles Python to bytecode, and is compatible with the standard Python implementation CPython.

In this talk I explain how I picked up this hobby, and where it might be going. I also talk about various Haskell features which underpin the two implementations.

****

## Functional Graphics in Python

An advanced lecture in COMP10001 Foundations of Computing at The University of Melbourne, 2013.

[Slides](/docs/functional_graphics_python.pdf)

#### Abstract

The conventional way to represent computer graphics is with a two-dimensional array of pixels. In these slides we illustrate an alternative approach using functions from continuous coordinates to pixel values. Apart from being a neat idea, this representation provides great expressiveness for image transformations, and allows us to do impressive things with only a small amount of code. In these slides we use Python, but the same idea can be applied in any language with higher-order functions. 

****

## Open recursion and fixed points (in Scala)

The Melbourne Scala Users Group, Monday 25 July 2011.

[Slides](/docs/open_recursion.pdf)

#### Abstract

Classes in object oriented languages combine many different programming language features into a single abstraction. One of the key features is open recursion - the recursion on the self parameter which provides a late binding mechanism on method calls. In these slides we show how the same kind of extensibility can be achieved with ordinary higher order functions and fixed points (no classes). We use Scala to demonstrate the key ideas, but they can be easily translated to any language with higher-order functions.

* [simple closed recursive form](/files/fib_closed.scala)
* [implicit open recursion using classes](/files/fib_class.scala)
* [explicit open recursive form using higher-order functions](/files/fib_open.scala)

****

## The road to dependent types 

The Melbourne Scala Users Group, Monday 28 March 2011.

[Slides](/docs/dependent_types.pdf)

#### Abstract

[Barendregt's Lambda Cube](http://en.wikipedia.org/wiki/Lambda_cube) provides a neat characterisation of various kinds of typed Lambda Calculi. In these slides I show how we can explore the axes of the Lambda Cube by studying families of languages indexed by (possibly other) languages. For example the Simply Typed Lambda Calculus supports terms indexed by terms, whereas the Polymorphic Lambda Calculus extends this with terms indexed by types. Taking this idea further we can also consider types indexed by types and finally arrive at types indexed by terms, which is what most people call Dependent Types.

****

## Haskell bindings to MPI

The Melbourne Functional Programming Union, Friday 30 July 2010.

[Slides](/docs/mpi_bindings.pdf)

#### Abstract

MPI (the Message Passing Interface) is a popular communication protocol for distributed parallel programming, providing both point-to- point and collective communication operators. Traditionally MPI is used in high-performance computing applications written in imperative languages, such as C, C++ and Fortran. I've recently been working on a Haskell binding to MPI via C and the Foreign Function Interface (FFI).  One of the main challenges in writing such a binding is deciding how to map the low-level C API into idiomatic Haskell. In this talk I provide a brief overview of MPI, and then discuss the hows and whys of my current implementation effort. 

****

## Berp - an implementation of Python 3 in Haskell

The Melbourne Functional Programming Union, Friday 18 June 2010.

[Slides](docs/Berp.pdf)

#### Abstract

Berp is an implementation of Python 3. At its heart is a translator, which takes Python code as input and generates Haskell code as output. The Haskell code is fed into a Haskell compiler (GHC) for compilation to machine code or interpretation as byte code. 

****

## Static Pattern Calculus

The Melbourne Functional Programming Union, Friday 22 January 2010.

[Slides](/docs/static_pattern_calculus.pdf)

#### Abstract

The Pattern Calculus extends the Lambda Calculus with first class patterns. Barry Jay recently published a book on the Pattern Calculus (which has been briefly discussed on this list), and I have been reading through it over the summer break. In this talk I give an overview of the term rewriting system which underpins the calculus. The term rewriting system is developed in four stages in the book. The first stage is the Lambda Calculus which is well known. The second stage is the Compound Calculus which is essentially a core version of LISP. The third stage is the Static Pattern Calculus which adds static patterns. The fourth stage is the Dynamic Pattern Calculus which adds dynamic patterns. This talk covers the static variant of the calculus only.

****

## Monads in Scala

The Melbourne Scala Users Group, Monday 26 October 2009.

[Slides](/docs/scala_monads.pdf)

#### Abstract

Monads are a concept from Category Theory which has found many applications in both programming language theory and programming practice (particularly in functional languages such as Haskell). In this talk I discuss the way that monads underpin the for-comprehension notation used in Scala, and provide a simple term language evaluator as an example. I also discuss how the quest for modular denotational semantics in programming language theory led to the use of monads in programming practice.

****

## Parser combinators in Scala

The Melbourne Scala Users Group, Monday 22 June 2009.

[Slides](/docs/scala_parser_combinators.pdf)

#### Abstract

Parser combinators provide an elegant way to specify parsing functions declaratively. Parsers written in this style have a structure which closely follows the EBNF grammar for the accepted language. This results in concise programs which can be readily checked against their formal specifications. Parsers are written entirely in the host language (in this case Scala), which avoids the need for additional languages and tools and their concomitant problems for software development. In this talk I will demonstrate Scala's parser combinator library by constructing a parser for (a non-trivial subset of) XML. If time permits, I will also discuss the key ideas which underpin the implementation of parser combinators, and their historical roots in functional programming.

****

## Applicative Functors

The Melbourne Functional Programming Union, Friday 3 July 2009.

[Slides](/docs/applicative.pdf).

#### Abstract

Applicative functors provide a programming abstraction which sits between functors and monads. They capture a common pattern of programming with "effects", which do not require the full expressiveness of monads.

One of the benefits of applicative functors over monads is that they allow *effectful* code to be written in direct style. For example, when using monadic parser combinators such as Parsec, one often writes code like this:

<pre><code>do { x <- e1; y <- e2; z <- e3; return (f x y z) }</code></pre>

where e1,e2,e3 are themselves parsers and f is some *pure* function which builds the result of the overall parser.

With applicative functors the same parser can be written like so:

<pre><code>f <$> e1 <*> e2 <*> e3</code></pre>

which resembles ordinary function application (hence the name *applicative*).

In cases like this, the monadic style obscures the important structure of the code, and requires redundant naming of the results of sub-computations (the x,y,z variables). Furthermore, in such cases the full expressiveness of monads is not warranted, and it is arguably better to use less expressive constructs where possible.

Applicative functors were introduced in the paper *Applicative Programming with Effects* (2008) by McBride and Paterson. 

****

## Simple graph reduction with visualisation

The Melbourne Functional Programming Union, Friday 12 May 2009.

[Slides](/docs/graphreduction.pdf)

#### Abstract

In 2008, whilst teaching the fourth year functional programming subject, I wrote a little functional language called miniFP for demonstration purposes. Near the end of the course we covered graph reduction. I extended miniFP to compile to a simple graph reduction system, implemented in C. One of the more interesting and useful aspects of the system is that it can produce pictures of the graph at each step of reduction. This allows the user to visualise the graph reduction process. It is especially helpful for understanding how cyclic data structures can be produced by 'tying the knot', which is a difficult topic to explain on paper. In this talk I outline some of the interesting aspects of the implementation, and show how it can be used.

****

## Stack tracing in Haskell - an exploration of the design space

The Haskell Implementors Workshop, Saturday 5 2009, Edinburgh, Scotland.

[Slides](/docs/stack_tracing.pdf)

[Video from the Haskell Implementors Workshop](http://www.vimeo.com/6575114)

#### Abstract

The development of debugging tools for Haskell has tended to lag behind its imperative peers. The main technical reason for this is the difficulty of reconciling the operational behaviour of code under lazy evaluation with its static source description. Recently GHC has incorporated a breakpoint debugger into its byte-code interpreter. Experience with the debugger suggests that, while it is a useful addition to the programmer's tool-set, the problems associated with lazy evaluation are (unsurprisingly) still present. Users often find it difficult to understand the chain of causation that leads to a particular expression being evaluated. Normally the chain of causation is represented as a stack of procedure invocations, but this is difficult in Haskell because the context in which a function application is constructed can differ considerably with the context in which it is evaluated. In this talk I will address the question "How can stack tracing be done in Haskell?". The structure of the talk will consist of three main parts: 1) the desirable features of a stack trace; 2) the current state of the art; 3) implementation issues, focusing on GHC. Overall I hope to sketch out the design space and spur interest in possible future implementation efforts.

****

## Continuations

The Melbourne Functional Programming Union, Friday 3 October 2008.

[Slides](/docs/continuations.pdf)

#### Abstract

Continuations are a method of reifying the evaluation context of a term within some computation. Intuitively, the continuation describes what may become of the value of the term in the overall computation. In this sense, a continuation provides a concrete representation of program control, and allows it to be manipulated. This "purely functional" account of program control has many useful results, such as: the extension of denotational semantics from the lambda calculus to (constructs from) conventional imperative languages, such as jumps; techniques for compiling high-level languages to machine code; and first class control operators, such as exception handlers. In recent times, more refined versions of continuations have emerged, such as the delimited continuations (or subcontinuations), which reify only a part of the evaluation context of a term. This talk covers: the continuation passing style (CPS); the history of continuations; some interesting applications of continuations; the use of continuations in the "direct style" of programming via primitives such as call/cc of Scheme; and connections to logic.
