---
title: Notes
notes: True
---

### [Ten rules of etiquette for scientific code](/scientific_software_etiquette.html)

Ten things to think about when writing scientific code to make it more usable and well mannered.

****

### [Introduction to Unix](/docs/intro_to_unix.pdf)

These notes were written to introduce new VLSCI users to the fundamental ideas of Unix. The notes cover
essential concepts such as the shell, the filesystem and processes. Some of the details are VLSCI specific,
but most of the material is applicable to any unix system (Linux, OS X, Solaris, etc).

****

### [Melbourne Programming Languages Workshop 2013](/melbPL2013.html)

To celebrate the presence of Phil Wadler, Gilad Bracha and Daniel Spiewak in Australia (to speak at the YOW! 2013 conference) we organised a local Programming Languages Research Workshop in Melbourne. We had a great turnout and a very impressive lineup of talks. 

****

### [Publishing Bioinformatics Software](/bioinfPubs.html)

A non-exhaustive list of places to publish papers on bioinformatics software. 

****

### [Melbourne Programming Languages Workshop 2011](/melbPL2011.html)

To celebrate the presence of Simon Peyton Jones and John Hughes in Australia (to speak at the YOW! 2011 conference) we organised a local Programming Languages Research Workshop in Melbourne. We had a great turnout and a very impressive lineup of talks. 

****

### [Linux Perf Events (Performance Counters)](/linuxPerfEvents.html)

Some notes I've taken about Linux Performance Counters whilst working on the linux-perf Haskell library.

****

### [Building SPRNG](/sprng.html)

Instructions for building the SPRNG Psuedo Random Number Library on OS X Snow Leopard (although probably relevant to Linux too).

****

### [Lagged Fibonacci Generators](/lfg.html)

Some comments on Lagged Fibonacci Generators (for pseudo random numbers) and their implementation in SPRNG.

****

### [Tools for testing RNGs](/rng_test.html)

Tools for testing pseudo random number generators.

****

### [Fixed points](/fixed_points.html)

Given a function f, if there exists a value x, such that x = f(x), then we say that x is a fixed point of f. A function may have zero fixed points, or it may have one, or many. In fact, a function can have infinitely many fixed points. These notes explore the idea of fixed points in the semantics of programming languages.

****

### [The Scott Encoding of data types into the lambda calculus](/scott_encoding.html)

The purpose of the Scott encoding (SE) is to transform programs with algebraic data types into pure lambda calculus (LC) terms. This is a simple and effective way to compile more full-featured languages into LC. The key idea in SE is to turn data constructors into functions which select from a set of alternatives, thus encoding the behaviour of case statements.

****

### [The Pure Lambda Calculus](/pure_lambda_calculus.html)

Church's Lambda Calculus is one of the cornerstones of theoretical Computer Science. These notes describe the syntax and semantics of the Pure Lambda Calculus (the simplest version of the lambda calculus). They also define some of the key terms used in the field.

****

### [Monad Transformers](/docs/MonadTransformers.lhs)

Monad transformers are type constructors which are parameterised by monads. A monad transformer applied to a monad yields a (new) monad. Monad transformers are of great utility in functional programming, as they allow us to build libraries for modular semantic features. In these notes we show that the Parser monad can be reconstructed from the Maybe monad (aka failure monad), and a state monad transformer.

****

### [Indexing for fast search in Python](/docs/indexing.pdf)

These notes discuss the use of an index to improve the performance of searching in CSV data. The notes use Python and demonstrate the use of dictionaries, pickling and reading CSV files.

****

### [Binary search trees in Python](/docs/binary_search_trees.pdf)

These notes illustrate a simple way to implement a dictionary using binary search trees in Python. Performance is compared with linear search and Python's built in dictionaries.

****

### [Recursion in Python](/docs/recursion.pdf)

These notes discuss the important concept of recursion. Topics discussed are: divide and conquer problem solving, recursive data structures (binary search trees and XML), Python programming.

****

### [HTTP cookies](/docs/cookies.pdf)

These notes explain the motivation for cookies in the HTTP protocol, and show simple use cases in Python CGI scripts.

****

### [CGI - facilitating interactive web applications](/docs/cgi.pdf)

These notes explain the Common Gateway Interface, more commonly known as CGI. They discuss the HTTP protocol and show how CGI scripts communicate with clients on the web. Simple examples are demonstrated in Python.

****

### [HTML forms - a user interface to CGI applications](/docs/htmlforms.pdf)

These notes discuss how HTML forms can be used to created user interfaces to CGI applications on the web. The difference between the GET and POST commands of the HTTP protocol is discussed, and simple examples are demonstrated in Python.

****

### [A Haskell quiz](/docs/haskell_quiz.pdf)

Test your knowledge of Haskell in this quiz.

****

### [A Lambda Calculus quiz](/docs/lambda_calculus_quiz.pdf)

Test your knowledge of the Lambda Calculus in this quiz.

****

### [Ninety nine bottles of beer on the wall - a study in repetition](/docs/Ninety_nine_bottles_of_beer_on_the_wall_.pdf)

A tutorial for teaching looping to novice programmers, based on the well-known song *Ninety nine bottles of beer on the wall*.
