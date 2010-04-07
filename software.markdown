---
title: Software 
---

## buddha 

A declarative debugger for Haskell

#### Downloads

* [Main buddha page](http://www.cs.mu.oz.au/~bjpop/buddha).
* [Source repository on patch tag](http://patch-tag.com/r/bjpop/buddha).

    You can download the buddha source repository using darcs with this command:

    `darcs get http://patch-tag.com/r/bjpop/buddha/pullrepo buddha`

#### Description

buddha is a debugger for Haskell 98 programs. To be more precise, it is a declarative debugger. It presents the evaluation of a Haskell program as a series of equivalences, rather than the more traditional stepwise presentation of debuggers for imperative languages.

A typical debugging session involves a number of questions and answers. The questions are posed by the debugger, and the answers are provided by the user. The questions relate to the evaluation of function applications that were made during the execution of the program. Evaluation is presented in a high-level manner, for example, something like:

    Prelude 35 map 
       arg 1  = fst 
       arg 2  = [(True,1),(False,2)] 
       result = [True,False] 

The above statement means: the application of `map` to `fst` and `[(True,1),(False,2)]` evaluates to `[True,False]`. The text `Prelude 35` says that the function was defined on line `35` of the module called `Prelude`. The job of the user is to determine whether the evaluation of this application is correct in their intended interpretation of the program. Of course, we presume the user knows what their program is supposed to do!

buddha employs a source-to-source program transformation. The program to be debugged is transformed, compiled, and linked with a debugging library. The transformation rules are crafted in such a way that execution of the transformed program constitutes evaluation of the original (untransformed) program, plus construction of a semantics for that evaluation. The semantics that it produces is a tree with nodes that correspond to equivalences like the one above. Debugging is a traversal of that tree, searching for nodes that do not agree with the user's intention.

The main advantage of this style of debugging is that the user does not need to think about the operational semantics of their program. They only need to think in terms of high-level logical specifications (declarative semantics). This is especially useful in non-strict languages like Haskell, where the evaluation order of the program is very difficult to relate to the structure of the source code (even experts find the operational behaviour of their program hard to understand).

****

## bjpop-ray

A ray tracer written in Haskell.

#### Downloads

* [Source repository on patch tag](http://patch-tag.com/r/bjpop/bjpop-ray).

    You can download the bjpop-ray source repository using darcs with this command:

    `darcs get http://patch-tag.com/r/bjpop/bjpop-ray/pullrepo bjpop-ray`

#### Description

A ray tracer for 3D computer graphics using wxHaskell for the GUI. I wrote bjpop-ray primarily for teaching purposes. In particular I wanted to demonstrate to first-year university students that Haskell can be used for non-trivial (and interesting) projects.

#### Gallery 

[bjpop-ray gallery]($root/bjpopray.html).

****

## language-python

A lexer, parser and pretty printer for Python programs, written in Haskell.

#### Downloads

* [Cabal package on hackage](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/language-python).
* [Source repository on github](http://github.com/bjpop/language-python).

    You can download the language-python source repository using git with this command:

    `git clone git://github.com/bjpop/language-python.git`

#### Description

This package provides a parser (and lexer) for Python written in Haskell. It supports version 2 and 3 of Python. The parser is implemented using the happy parser generator, and the alex lexer generator. The package also provides a pretty printer, which makes it also suitable for generating Python code.

As a demonstration of the library, I have written a simple code colouring tool which converts Python source code into XHTML files. A CSS file is used to determine the presentation of the code (such as colours). 

#### Gallery

[An example coloured Python file]($root/html/pycol/lsystem.py.html).

****

## ministg

An interpreter for the small-step operational semantics of the STG machine.

#### Downloads

* [Project web page on haskell.org](http://www.haskell.org/haskellwiki/Ministg).
* [Cabal package on hackage](http://hackage.haskell.org/package/ministg).
* [Source repository on patch tag](http://patch-tag.com/r/bjpop/ministg).

    You can download the ministg source repository using darcs with this command:

    `darcs get http://patch-tag.com/r/bjpop/ministg/pullrepo ministg`

#### Description

Ministg is an interpreter for a high-level, small-step, operational semantics for the STG machine. The STG machine is the abstract machine at the core of GHC. The operational semantics used in Ministg is taken from the paper *Making a fast curry: push/enter versus eval/apply for higher-order languages* by Simon Marlow and Simon Peyton Jones. Ministg implements both sets of evaluation rules from the paper.

One of the main features of Ministg is the ability to record a trace of the execution steps as a sequence of html files. 


The example shows the execution of a program which sums a list of three integers, using the well-known space-leaky version of sum. Follow the *next* and *previous* links to step forwards and backwards through the trace.

The main reason I wrote Ministg is to explore various extensions to the STG machine. The current release features a simple extension in the form of call-stack tracing, which is strongly influenced by the cost-centre stacks of GHC. I expect it will also be a useful tool for people who are interested in learning more about the STG machine.

Here is an example ministg program for calculating the factorial of 7:

    fac = FUN (x ->
             case eqInt x zero of {
                True -> one;
                False -> let { s = THUNK(subInt x one);
                               rec = THUNK(fac s) }
                         in multInt x rec
             });

    main = THUNK (fac seven)

#### Gallery  

[An example program trace]($root/html/ministg/step0.html).

****

## py-lsystem

An L-System (Lindenmayer system) in Python.

#### Downloads

* [Source repository on patch tag](http://patch-tag.com/r/bjpop/py-lsystem).

    You can download the py-lsystem source repository using darcs with this command:

    `darcs get http://patch-tag.com/r/bjpop/py-lsystem/pullrepo py-lsystem`

#### Description

L-Systems (wikipedia entry) are term rewriting languages which have interesting interpretations in computer graphics.

Originally I wrote this program for a guest lecture in a first-year university Python course, but I have subsequently been using it to motivate computing to high-school students.

A selection of example L-System rules and the resulting images produce by py-lsystem can be seen in my L-System gallery.

#### Gallery

[L-System gallery]($root/lsystem.html).

****

## Scala XML parser

#### Downloads

* [Source repository on patch tag](http://patch-tag.com/r/bjpop/scala-xml-parser).

    You can download the scala-xml-parser source repository using darcs with this command:

    `darcs get http://patch-tag.com/r/bjpop/scala-xml-parser/pullrepo scala-xml-parser`

#### Description

This is a parser for a non-trivial subset of XML, written using parser combinators in Scala.

I wrote this program to demonstrate the use of parser combinators in Scala, as part of a talk I did at the Melbourne Scala Users group. See my [slides from the talk]($root/docs/scala_parser_combinators.pdf) for more details.

For comparison, here is the same parser written in Haskell using the Parsec combinators: repository on patch tag.

****

## hatchet

A type checker for Haskell

#### Downloads

* [Source tar ball](http://www.cs.mu.oz.au/~bjpop/hatchet/src/hatchet.tar.gz).
* [Online documentation](http://www.cs.mu.oz.au/~bjpop/hatchet/onlinedocs/hatchet/index.html).

#### Description

Hatchet is a type checking and inference tool for Haskell 98, written in (almost) Haskell 98. It is based on the Typing Haskell in Haskell work of Mark Jones.

I originally wrote hatchet as part of a program transformation tool for debugging Haskell. Ultimately that tool was rewritten to avoid the need for type information, so hatchet was forked off as a separate project. Eventually hatchet made its way into other projects, such as the [JHC Haskell compiler](http://repetae.net/computer/jhc/).

****

## baskell

An interpreter for a small functional language with type inference.

#### Downloads

* [Source repository on patch tag](http://patch-tag.com/r/bjpop/baskell).

    You can download the baskell source repository using darcs with this command:

    `darcs get http://patch-tag.com/r/bjpop/baskell/pullrepo baskell`

#### Description

Baskell is an interpreter for a small functional language. It features Hindley/Milner type inference, strict and lazy evaluation and an interactive REPL (read-eval-print-loop) command line.

For the history buffs out there, the [PUGS implementation of Perl 6](http://www.pugscode.org/) began with some parts of baskell in it.

Here is an example baskell program for calculating the factorial of a number, using Turing's fixed point combinator to implement recursion (baskell has built in recursion too, but this example shows that you can also do without):

    fix = (\x -> \y -> y (x x y)) (\x -> \y -> y (x x y));
    fac = fix facNonRec;
    facNonRec
       = \f -> \n ->
            ite (eqI n 0)
                1
                (mult n (f (sub n 1)));

****

## image-transform

Functional manipulation of 2D images.

#### Downloads

* [Source repository on patch tag](http://patch-tag.com/r/bjpop/image-transform).

    You can download the image-transform source repository using darcs with this command:

    `darcs get http://patch-tag.com/r/bjpop/image-transform/pullrepo image-transform`

#### Description

This is a small program for manipulating 2D computer images. It was written for a project that I gave to first-year Haskell students.

It can generate interesting effects, such as simulated water ripples, and geometric distortions.

#### Gallery

[Image transform gallery]($root/imgtrans.html).

****

## terpie-llvm

A small imperative language compiler with LLVM back-end.

#### Downloads

* [Source repository on patch tag](http://patch-tag.com/r/bjpop/terpie-llvm).

    You can download the terpie-llvm source repository using darcs with this command:

    `darcs get http://patch-tag.com/r/bjpop/terpie-llvm/pullrepo terpie-llvm`

#### Description

Terpie is a small imperative language that I created when teaching the fourth-year functional programming course. Terpie-llvm is compiler for the language which uses the LLVM back-end. I made terpie-llvm for a talk that I gave at the FPU on the Haskell LLVM bindings.

Here is an example terpie program for calculating the factorial of 10:

    def fac (x) {
        ans = 1;
        y = 1;
        while (y <= x) {
           ans = ans * y;
           y = y + 1;
        }
        return ans;
    }

    def main () { return fac(10); }

****

## minifp

A small functional language with different backends.

#### Downloads

* [Source repository on patch tag](http://patch-tag.com/r/bjpop/minifp).

    You can download the minifp source repository using darcs with this command:

    `darcs get http://patch-tag.com/r/bjpop/minifp/pullrepo minifp`

#### Description

Minifp is a small functional programming language that I created for teaching the fourth-year functional programming subject. It features Hindley/Milner style type inference, a compiler to pure lambda calculus, and a compiler to a graph reduction system (written in C).

Here is an example minifp program for calculating the factorial of 8:

    let plus = fix (\rec -> \m -> \n ->
       if iszero m
          then n
          else succ (rec (pred m) n)
       fi
       end end end)
    in
    let mult = fix (\rec -> \m -> \n -> 
       if iszero m 
          then 0
          else plus n (rec (pred m) n)
       fi
       end end end)
    in     
    let fac = fix (\rec -> \n ->
       if iszero n
          then 1
          else mult n (rec (pred n))
       fi
       end end)
    in fac 8 end end end

****
