---
title: Writing
---

## Theses

* PhD thesis: [A Declarative Debugger for Haskell](/docs/BerniePope.PhD.Thesis.pdf). Completed December 2006.

    This thesis is about the design and implementation of a debugging tool which helps Haskell programmers understand why their programs do not work as intended. The traditional debugging technique of examining the program execution step-by-step, popular with imperative languages, is less suitable for Haskell because its unorthodox evaluation strategy is difficult to relate to the structure of the original program source code. We build a debugger which focuses on the high-level logical meaning of a program rather than its evaluation order. This style of debugging is called declarative debugging, and it originated in logic programming languages. At the heart of the debugger is a tree which records information about the evaluation of the program in a manner which is easy to relate to the structure of the program. Links between nodes in the tree reflect logical relationships between entities in the source code. An error diagnosis algorithm is applied to the tree in a top-down fashion, searching for causes of bugs. The search is guided by an oracle, who knows how each part of the program should behave. The oracle is normally a human - typically the person who wrote the program - however, much of its behaviour can be encoded in software.

* Honours thesis: [Buddha: A Declarative Debugger for Haskell](/docs/BerniePope.Hons.Thesis.pdf). Completed June 1998. 

    Due to their reliance on the execution order of programs, traditional debugging techniques are not well suited to locating the source of logical errors in programs written in lazy functional languages. We describe the implementation of a declarative debugger for the programming language Haskell, which assists the location of logical errors based on the declarative semantics of program definitions. The implementation is based on the Hugs interpreter, and both solidifies previous work in the field and extends it to incorporate features typical of many modern lazy functional languages.

## Papers

* [Monadic Parsing: A Case Study](/docs/monad_parse.pdf). Co-authored with Mark Wielaard and Simon Taylor. 

    One of the selling points for functional languages is the ease with which parsers for simple languages can be expressed. In this report we give an introduction to monadic and operator precedence parsing in functional languages, using the parsing of propositional logic formulas as an example.

* [Haskell for Miranda Programmers](/docs/mira2hask.pdf). Co-authored with Kevin Glynn. 

    This document is designed to help programmers with a knowledge of Miranda move to Haskell as quickly and as painlessly as possible.

* [A tour of the Haskell Prelude](/docs/haskell.tour.tar.gz). A guide to the functions, operators and classes of the Haskell 98 Prelude.

    This paper serves as a reference guide for the functions and types in the Haskell Prelude (the standard library which is implicitly imported into all Haskell modules). The intended audience is people who are starting to learn Haskell. It has proven particularly useful in teaching Haskell at university level, and a HTML version of the document is widely available on the Internet.

* [Specialisation of Higher-Order Functions for Debugging](/docs/specialise.pdf). Co-authored with Lee Naish.

    Because functions are abstract values without convenient print representations, implementing debuggers which support higher-order code is a challenge. We present an algorithm for statically specialising higher-order functions and encoding higher-order values to allow printing. We define our algorithm for a small functional language and discuss how it may be extended to support sophisticated features of modern functional programming languages. This research forms part of a project to build a declarative debugger for Haskell, based primarily on source-to-source transformation.

    [Slides for my talk at WFLP 01](/docs/spec.talk.pdf).

* [Reification in Haskell (draft)](/docs/reify.pdf) Co-authored with Lee Naish.

    In this paper we investigate a limited form of reification in Haskell. We incrementally develop a meta-representation with a view to expressing the lazy evaluation and cyclic sharing of first-order values. We implement most of our facility within Haskell for portability and make extensive use of Haskell's type classes. We show how this metarepresentation can be derived for algebraic data types, and illustrate its use through a generic printing facility and a re-evaluation scheme. We also briefly explore a means for converting meta-representations back into their object level values.

    [Slides for my talk at IFL 01](/docs/reify.talk.pdf).

* [A program transformation for Declarative Debugging (draft)](/docs/pope.naish.apepm.pdf) Co-authored with Lee Naish.

    We present a declarative debugger for lazy functional programs, based primarily on program transformation. The debugger is designed to assist the detection and location of errors in programs which produce incorrect results for some or all of their inputs. We define the transformation over a core functional language, and pay close attention to the treatment of curried function applications. We consider the space complexity of the debugger, and sketch a method for improvement. We use Haskell as the target of the transformation, and consider extending the transformation to support the entire language.

* [A program transformation for debugging Haskell 98](/docs/acsc.pdf). Co-authored with Lee Naish, presented at ACSC 2003.

    We present a source-to-source transformation of Haskell 98 pro-grams for the purpose of debugging. The source code of a program is transformed into a new program which, when executed,computes the value of the original program and a high-level semantics for that computation. The semantics is given by a tree whose nodes represent function applications that were evaluated during execution. This tree is useful in situations where a high-level view of a computation is needed, such as declarative debugging. The main contribution of the paper is the treatment of higher-order functions, which have previously proven difficult to support in declarative debugging schemes.

* [Practical Aspects of Declarative Debugging in Haskell 98](/docs/PopeNaishPPDP.pdf). Co-authored with Lee Naish, presented at PPDP 2003.

    Non-strict purely functional languages pose many challenges to the designers of debugging tools. Declarative debugging has long been considered a suitable candidate for the task due to its abstraction over the evaluation order of the program, although the provision of practical implementations has been lagging. In this paper we discuss the solutions used in our declarative debugger for Haskell to tackle the problems of printing values, memory usage and I/O. The debugger is based on program transformation, although much leverage is gained by interfacing with the runtime environment of the language implementation through a foreign function interface.

* [Declarative Debugging with buddha](/docs/Afp.pdf). In Varmo Vene and Tarmo Uustalu, editors, Advanced Functional Programming, 5th International School, AFP 2004, volume 3622 of Lecture Notes in Computer Science, pages 331-357. Springer Verlag, September 2005.

    Haskell is a very safe language, particularly because of its type system. However there will always be programs that do the wrong thing. Programmer fallibility, partial or incorrect specifications and typographic errors are but a few of the reasons that make bugs a fact of life. This paper is about the use and implementation of a debugger, called buddha, which helps Haskell programmers understand why their programs misbehave. Traditional debugging tools that examine the program execution step-by-step are not suitable for Haskell because of its unorthodox evaluation strategy. Instead, a different approach is taken which abstracts away the evaluation order of the program and focuses on its high-level logical meaning. This style of debugging is called Declarative Debugging, and it has its roots in the Logic Programming community. At the heart of the debugger is a tree which records information about the evaluation of the program in a manner which is easy to relate to the structure of the source code. It resembles a call graph annotated with the arguments and results of function applications, shown in their most evaluated form. Logical relationships between entities in the source are reflected in the links between nodes in the tree. An error diagnosis algorithm is applied to the tree in a top-down fashion in the search for causes of bugs.

    Slides for the talk are available: [Set 1](/docs/Afp.slides1.pdf), [Set 2](/docs/Afp.slides2.pdf) and [Set 3](/docs/Afp.slides3.pdf).

* [Getting a Fix from the Right Fold](http://www.haskell.org/sitewiki/images/1/14/TMR-Issue6.pdf). A tutorial in Issue 6 of the Monad Reader online Haskell magazine. January 2007.

    What can you do with foldr? This is a seemingly innocent question that will confront most functional programmers at some point in their life. I was recently posed a folding challenge by a teaching colleague. The challenge was to write dropWhile using foldr. We gave the challenge to our first-year students, and awarded a small prize to the author of the first working solution. I have since passed the challenge on to other functional friends, and the results have been illuminating. That prompted me to write this article.

* [A Lightweight Interactive Debugger for Haskell](/docs/ghci-debug.pdf). Co-authored with Simon Marlow, Jose Iborra, and Andy Gill. The Haskell Workshop 2007, June 2007.

    This paper describes the design and construction of a Haskell source-level debugger built into the GHCi interactive environment. We have taken a pragmatic approach: the debugger is based on the traditional stop-examine-continue model of online debugging, which is simple and intuitive, but has traditionally been shunned in the context of Haskell because it exposes the lazy evaluation order. We argue that this drawback is not as severe as it may seem, and in some cases is an advantage. The design focuses on availability: our debugger is intended to work on all programs that can be compiled with GHC, and without requiring the programmer to jump through additional hoops to debug their program. The debugger has a novel approach for reconstructing the type of runtime values in a polymorphic context. Our implementation is light on complexity, and was integrated into GHC without significant upheaval.

* [Step inside the GHCi debugger](/docs/ghci-debug.monad.reader.pdf). A tutorial in Issue 10 of the Monad Reader online Haskell magazine. April 2008.

    Major releases of GHC are highly anticipated events, especially because of all the exciting new features they bring. The 6.8 series was a particularly impressive example, which came with lots of goodies, including a shiny new debugger. In this article we take the debugger out for a test run, and see what it can do.

* [High Performance Haskell with MPI](/docs/haskell-mpi.monad.reader.pdf). Co-authored with Dmitry Astapov, in Issue 19 of the Monad Reader online Haskell magazine. October 2011.

In this article, we give a brief overview of the Haskell-MPI library and show how it can be used to write distributed para
llel programs. We use the trapezoid method for approximating definite integrals as a motivating example and compare the performance of an implementation using Haskell-MPI to three variations of the same algorithm: a sequential Haskell program, a multi-threaded Haskell program, and a C program also using MPI.

## Slides

* [Open recursion and fixed points (in Scala)](/docs/open_recursion.pdf). Presented at the Melbourne Scala Users Group, Monday 25 July 2011.

    Classes in object oriented languages combine many different programming language features into a single abstraction. One of the key features is open recursion - the recursion on the *self* parameter which provides a late binding mechanism on method calls. In these slides we show how the same kind of extensibility can be achieved with ordinary higher order functions and fixed points (no classes). We use Scala to demonstrate the key ideas, but they can be easily translated to any language with higher-order functions.

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


## Notes

* [Fixed points](/docs/fixed_points.txt).

    Given a function f, if there exists a value x, such that x = f(x), then we say that x is a fixed point of f. A function may have zero fixed points, or it may have one, or many. In fact, a function can have infinitely many fixed points. These notes explore the idea of fixed points in the semantics of programming languages.

* [The Scott Encoding of data types into the lambda calculus](/docs/scott_encoding.txt).

    The purpose of the Scott encoding (SE) is to transform programs with algebraic data types into pure lambda calculus (LC) terms. This is a simple and effective way to compile more full-featured languages into LC. The key idea in SE is to turn data constructors into functions which select from a set of alternatives, thus encoding the behaviour of case statements.

* [The Pure Lambda Calculus](/docs/pure_lambda_calculus.txt).

    Church's Lambda Calculus is one of the cornerstones of theoretical Computer Science. These notes describe the syntax and semantics of the Pure Lambda Calculus (the simplest version of the lambda calculus). They also define some of the key terms used in the field.

* [Monad Transformers](/docs/MonadTransformers.lhs).

    Monad transformers are type constructors which are parameterised by monads. A monad transformer applied to a monad yields a (new) monad. Monad transformers are of great utility in functional programming, as they allow us to build libraries for modular semantic features. In these notes we show that the Parser monad can be reconstructed from the Maybe monad (aka failure monad), and a state monad transformer.

* [Indexing for fast search in Python](/docs/indexing.pdf).

    These notes discuss the use of an index to improve the performance of searching in CSV data. The notes use Python and demonstrate the use of dictionaries, pickling and reading CSV files.

* [Binary search trees in Python](/docs/binary_search_trees.pdf).

    These notes illustrate a simple way to implement a dictionary using binary search trees in Python. Performance is compared with linear search and Python's built in dictionaries.

* [Recursion in Python](/docs/recursion.pdf).

    These notes discuss the important concept of recursion. Topics discussed are: divide and conquer problem solving, recursive data structures (binary search trees and XML), Python programming.

* [HTTP cookies](/docs/cookies.pdf).

    These notes explain the motivation for cookies in the HTTP protocol, and show simple use cases in Python CGI scripts.

* [CGI - facilitating interactive web applications](/docs/cgi.pdf).

    These notes explain the Common Gateway Interface, more commonly known as CGI. They discuss the HTTP protocol and show how CGI scripts communicate with clients on the web. Simple examples are demonstrated in Python.

* [HTML forms - a user interface to CGI applications](/docs/htmlforms.pdf).

    These notes discuss how HTML forms can be used to created user interfaces to CGI applications on the web. The difference between the GET and POST commands of the HTTP protocol is discussed, and simple examples are demonstrated in Python.

* [A Haskell quiz](/docs/haskell_quiz.pdf).

    Test your knowledge of Haskell in this quiz.

* [A Lambda Calculus quiz](/docs/lambda_calculus_quiz.pdf).

    Test your knowledge of the Lambda Calculus in this quiz.

* [Ninety nine bottles of beer on the wall - a study in repetition](/docs/Ninety_nine_bottles_of_beer_on_the_wall_.pdf).

    A tutorial for teaching looping to novice programmers, based on the well-known song *Ninety nine bottles of beer on the wall*.
