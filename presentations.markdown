---
title: Presentations
---

* [Functional Graphics in Python](/docs/functional_graphics_python.pdf). An advanced lecture in COMP10001 Foundations of Computing at The University of Melbourne, 2013.

    The conventional way to represent computer graphics is with a two-dimensional array of pixels. In these slides we illustrate an alternative approach using functions from continuous coordinates to pixel values. Apart from being a neat idea, this representation provides great expressiveness for image transformations, and allows us to do impressive things with only a small amount of code. In these slides we use Python, but the same idea can be applied in any language with higher-order functions. 

* [Open recursion and fixed points (in Scala)](/docs/open_recursion.pdf). Presented at the Melbourne Scala Users Group, Monday 25 July 2011.

    Classes in object oriented languages combine many different programming language features into a single abstraction. One of the key features is open recursion - the recursion on the self parameter which provides a late binding mechanism on method calls. In these slides we show how the same kind of extensibility can be achieved with ordinary higher order functions and fixed points (no classes). We use Scala to demonstrate the key ideas, but they can be easily translated to any language with higher-order functions.

    * [simple closed recursive form](/files/fib_closed.scala)
    * [implicit open recursion using classes](/files/fib_class.scala)
    * [explicit open recursive form using higher-order functions](/files/fib_open.scala)

* [The road to dependent types](/docs/dependent_types.pdf). Presented at the Melbourne Scala Users Group, Monday 28 March 2011.

    [Barendregt's Lambda Cube](http://en.wikipedia.org/wiki/Lambda_cube) provides a neat characterisation of various kinds of typed Lambda Calculi. In these slides I show how we can explore the axes of the Lambda Cube by studying families of languages indexed by (possibly other) languages. For example the Simply Typed Lambda Calculus supports terms indexed by terms, whereas the Polymorphic Lambda Calculus extends this with terms indexed by types. Taking this idea further we can also consider types indexed by types and finally arrive at types indexed by terms, which is what most people call Dependent Types.

* [Static Pattern Calculus](/docs/static_pattern_calculus.pdf). Presented at the Melbourne FPU, Friday 22 January 2010.

    The Pattern Calculus extends the Lambda Calculus with first class patterns. Barry Jay recently published a book on the Pattern Calculus (which has been briefly discussed on this list), and I have been reading through it over the summer break. In this talk I give an overview of the term rewriting system which underpins the calculus. The term rewriting system is developed in four stages in the book. The first stage is the Lambda Calculus which is well known. The second stage is the Compound Calculus which is essentially a core version of LISP. The third stage is the Static Pattern Calculus which adds static patterns. The fourth stage is the Dynamic Pattern Calculus which adds dynamic patterns. This talk covers the static variant of the calculus only.

* [Monads in Scala](/docs/scala_monads.pdf). Presented at the Melbourne Scala Users Group, Monday 26 October 2009.

    Monads are a concept from Category Theory which has found many applications in both programming language theory and programming practice (particularly in functional languages such as Haskell). In this talk I discuss the way that monads underpin the for-comprehension notation used in Scala, and provide a simple term language evaluator as an example. I also discuss how the quest for modular denotational semantics in programming language theory led to the use of monads in programming practice.

* [Parser combinators in Scala](/docs/scala_parser_combinators.pdf). Presented at the Melbourne Scala Users Group, Monday 22 June 2009.

    Parser combinators provide an elegant way to specify parsing functions declaratively. Parsers written in this style have a structure which closely follows the EBNF grammar for the accepted language. This results in concise programs which can be readily checked against their formal specifications. Parsers are written entirely in the host language (in this case Scala), which avoids the need for additional languages and tools and their concomitant problems for software development. In this talk I will demonstrate Scala's parser combinator library by constructing a parser for (a non-trivial subset of) XML. If time permits, I will also discuss the key ideas which underpin the implementation of parser combinators, and their historical roots in functional programming.

* [Continuations](/docs/continuations.pdf). Presented at the Melbourne FPU, Friday 3 October 2008.

    Continuations are a method of reifying the evaluation context of a term within some computation. Intuitively, the continuation describes what may become of the value of the term in the overall computation. In this sense, a continuation provides a concrete representation of program control, and allows it to be manipulated. This "purely functional" account of program control has many useful results, such as: the extension of denotational semantics from the lambda calculus to (constructs from) conventional imperative languages, such as jumps; techniques for compiling high-level languages to machine code; and first class control operators, such as exception handlers. In recent times, more refined versions of continuations have emerged, such as the delimited continuations (or subcontinuations), which reify only a part of the evaluation context of a term. This talk covers: the continuation passing style (CPS); the history of continuations; some interesting applications of continuations; the use of continuations in the "direct style" of programming via primitives such as call/cc of Scheme; and connections to logic.

* [Applicative Functors](/docs/applicative.pdf). Presented at the Melbourne FPU, Friday 3 July 2009.

    Applicative functors provide a programming abstraction which sits between functors and monads. They capture a common pattern of programming with "effects", which do not require the full expressiveness of monads.

    One of the benefits of applicative functors over monads is that they allow *effectful* code to be written in direct style. For example, when using monadic parser combinators such as Parsec, one often writes code like this:

    <pre><code>do { x <- e1; y <- e2; z <- e3; return (f x y z) }</code></pre>

    where e1,e2,e3 are themselves parsers and f is some *pure* function which builds the result of the overall parser.

    With applicative functors the same parser can be written like so:

    <pre><code>f <$> e1 <*> e2 <*> e3</code></pre>

    which resembles ordinary function application (hence the name *applicative*).

    In cases like this, the monadic style obscures the important structure of the code, and requires redundant naming of the results of sub- computations (the x,y,z variables). Furthermore, in such cases the full expressiveness of monads is not warranted, and it is arguably better to use less expressive constructs where possible.

    Applicative functors were introduced in the paper *Applicative Programming with Effects* (2008) by McBride and Paterson: Applicative Programming with Effects. 

* [Simple graph reduction with visualisation](/docs/graphreduction.pdf). Presented at the Melbourne FPU, Friday 12 May 2009.

    In 2008, whilst teaching the fourth year functional programming subject, I wrote a little functional language called miniFP for demonstration purposes. Near the end of the course we covered graph reduction. I extended miniFP to compile to a simple graph reduction system, implemented in C. One of the more interesting and useful aspects of the system is that it can produce pictures of the graph at each step of reduction. This allows the user to visualise the graph reduction process. It is especially helpful for understanding how cyclic data structures can be produced by 'tying the knot', which is a difficult topic to explain on paper. In this talk I outline some of the interesting aspects of the implementation, and show how it can be used.

* [Stack tracing in Haskell - an exploration of the design space](/docs/stack_tracing.pdf). Presented at the Haskell Implementors Workshop, Saturday 5 2009, Edinburgh, Scotland.

    The development of debugging tools for Haskell has tended to lag behind its imperative peers. The main technical reason for this is the difficulty of reconciling the operational behaviour of code under lazy evaluation with its static source description. Recently GHC has incorporated a breakpoint debugger into its byte-code interpreter. Experience with the debugger suggests that, while it is a useful addition to the programmer's tool-set, the problems associated with lazy evaluation are (unsurprisingly) still present. Users often find it difficult to understand the chain of causation that leads to a particular expression being evaluated. Normally the chain of causation is represented as a stack of procedure invocations, but this is difficult in Haskell because the context in which a function application is constructed can differ considerably with the context in which it is evaluated. In this talk I will address the question "How can stack tracing be done in Haskell?". The structure of the talk will consist of three main parts: 1) the desirable features of a stack trace; 2) the current state of the art; 3) implementation issues, focusing on GHC. Overall I hope to sketch out the design space and spur interest in possible future implementation efforts.

    [A video of my talk at the Haskell Implementors Workshop](http://www.vimeo.com/6575114).

* [Berp - an implementation of Python 3 in Haskell](/docs/Berp.pdf). Presented at the Melbourne FPU, Friday 18 June 2010.

    Berp is an implementation of Python 3. At its heart is a translator, which takes Python code as input and generates Haskell code as output. The Haskell code is fed into a Haskell compiler (GHC) for compilation to machine code or interpretation as byte code. 

* [Haskell bindings to MPI](/docs/mpi_bindings.pdf). Presented at the Melbourne FPU, Friday 30 July 2010.

    MPI (the Message Passing Interface) is a popular communication protocol for distributed parallel programming, providing both point-to- point and collective communication operators. Traditionally MPI is used in high-performance computing applications written in imperative languages, such as C, C++ and Fortran. I've recently been working on a Haskell binding to MPI via C and the Foreign Function Interface (FFI).  One of the main challenges in writing such a binding is deciding how to map the low-level C API into idiomatic Haskell. In this talk I provide a brief overview of MPI, and then discuss the hows and whys of my current implementation effort. 
