---
title: Software
---

## ROVER 

Read-pair overlap considerate variant-calling software for PCR-based massively parallel sequencing datasets.

#### Downloads

* [Source repository on github](https://github.com/bjpop/rover).

    You can download the ROVER source repository using git with this command:

    `git clone https://github.com/bjpop/rover` 

#### Description 

ROVER-PCR Variant Caller enables users to quickly and accurately identify
genetic variants from PCR-targeted, overlapping paired-end MPS datasets. The
open-source availability of the software and threshold tailorability enables
broad access for a range of PCR-MPS users.

This program is described in the paper [ROVER variant caller: read-pair overlap considerate variant-calling software
   applied to PCR-based massively parallel sequencing datasets](http://www.scfbm.org/content/9/1/3), published in Source Code for Biology and Medicine.

****

## SLURM job script generator

[An interactive job script generator for SLURM in a web page](/html/jobscript/slurm.html).

#### Downloads

* [Source repository on github](https://github.com/bjpop/jobscript_generator).

    You can download the source repository using git with this command:

    `git clone https://github.com/bjpop/jobscript_generator` 

#### Description 

The [SLURM resource manager]("http://slurm.schedmd.com/") is a popular
tool in high-performance computing. Users can describe their compute jobs
in terms of the resources they need, such as walltime, number of cores,
amount of RAM per core, and so on.

The job script generator is a web page which translates the details of
a compute job into a batch script for the SLURM system. The batch
script can then be used to submit a job via the sbatch command.

****

## Blip 

A bytecode compiler for Python 3.

#### Downloads

* [Source repository on github](https://github.com/bjpop/blip).

    You can download the Blip source repository using git with this command:

    `git clone https://github.com/bjpop/blip` 

#### Description 

Blip compiles Python 3 source files to bytecode. The output bytecode
is compatible with the CPython interpreter. For example, given a Python 3 
source file called `foo.py`, the command:

   `blip foo.py`

produces a bytecode file called `foo.pyc`. The bytecode can be executed
by passing it as an argument to a CPython interpreter:

   `python3 foo.pyc`

You can specify more than one python source file on the command line;
Blip will compile them all in sequence, stopping at the first error
encountered.

The Blip source tree also includes code for a program called `readpyc`,
which can be used for pretty printing the contents of `.pyc` files:

   `readpyc foo.pyc`

More details are on the [blip wiki](https://github.com/bjpop/blip/wiki).

****

## FAVR

Filtering and Annotation of Variants that are Rare.

#### Downloads

* [Source repository on github](https://github.com/bjpop/favr).

    You can download the FAVR source repository using git with this command:

    `git clone https://github.com/bjpop/favr` 

#### Description 

Characterizing genetic diversity through the analysis of massively parallel
sequence (MPS) data offers enormous potential in terms of our understanding of
predisposition to complex human disease. Great challenges remain, however,
regarding our ability to resolve those genetic variants that are genuinely
associated with disease from the millions of "bystanders" and artefactual
signals. FAVR is designed to assist in the resolution of some of these issues
in the context of rare germline variants by facilitating "platform-steered"
artefact filtering.

This program is described in the paper [FAVR (Filtering and Annotation of Variants that are Rare): methods to facilitate the analysis of rare germline genetic variants from massively parallel sequencing datasets](http://www.biomedcentral.com/1471-2105/14/65/abstract), published in BMC Bioinformatics.

****

## Python ray tracer

A simple ray tracer written in Python.

#### Downloads

* [Source repository on github](https://github.com/bjpop/pyray).

    You can download the source repository using git with this command:

    `git clone https://github.com/bjpop/pyray` 

#### Description 

This module implements a simple ray tracer. Its features are:

   - Multiple point (white) light sources.
   - Multiple coloured objects (spheres, planes).
   - Ambient illumination (simulating scattered light).
   - Diffuse illumination (simulating light scattering surfaces).
   - Specular illumination using the Phong model (simulating highlights).
   - Reflective illumination (simulating mirrored surfaces).
   - Shadows.

It was written for the purposes of demonstrating Ray Tracing in an 
Advanced Lecture of the University of Melbourne subject COMP10001
Foundations of Computing. As such the emphasis is on simplicity and
clarity over performance. There are many ways to make this program
faster, but usually at the expence of code readability.

#### Gallery

[pyray gallery](/pyray.html)

****

## Haskell linux-perf 

A Haskell library for reading and processing Linux performance counter trace-file data.

#### Downloads

* [Cabal package on hackage](http://hackage.haskell.org/package/linux-perf).
* [Source repository on github](https://github.com/bjpop/haskell-linux-perf).

    You can download the haskell linux-perf source repository using git with this command:

    `git clone https://github.com/bjpop/haskell-linux-perf` 

#### Documentaion

* [My notes about Linux performance counters](/linuxPerfEvents.html).

#### Description 

This library is for parsing, representing in Haskell and pretty printing the data file output of the Linux perf command. The perf command provides performance profiling information for applications running under the Linux operating system. This information includes hardware performance counters and kernel tracepoints.

Modern CPUs can provide information about the runtime behaviour of software through so-called [hardware performance counters](http://en.wikipedia.org/wiki/Hardware_performance_counter). Recent versions of the Linux kernel (since 2.6.31) provide a generic interface to low-level events for running processes. This includes access to hardware counters but also a wide array of software events such as page faults, scheduling activity and system calls. A userspace tool called perf is built on top of the kernel interface, which provides a convenient way to record and view events for running processes.

****

## js-turtle

[Turtle graphics in Javascript](/html/js-turtle/turtle.html).

[Logo](http://en.wikipedia.org/wiki/Logo_(programming_language)) is a popular choice
for teaching programming to beginners. Its most significant feature is *turtle graphics* which
provides a simple way to create graphical programs. js-turtle is a re-implementation of
turtle graphics using the Javascript language. It runs directly in a web page using the canvas
element of HTML5 for output.

****

## berp

A compiler and interpreter for Python 3.

#### Downloads

* [Cabal package on hackage](http://hackage.haskell.org/package/berp).
* [Source repository on github](http://github.com/bjpop/berp).

    You can download the berp source repository using git with this command:

    `git clone http://github.com/bjpop/berp.git` 


#### Documentaion

* [Wiki on github](http://wiki.github.com/bjpop/berp/).

#### Description

Berp is an implementation of Python 3. At its heart is a translator, which takes Python 
code as input and generates Haskell code as output. The Haskell code is fed into a 
Haskell compiler (GHC) for compilation to machine code or interpretation as byte code.

Berp provides both a compiler and an interactive interpreter. For the most part it 
can be used in the same way as CPython (the main Python implementation).

****

## haskell-mpi

A Haskell interface to the MPI distributed parallel library.

#### Downloads

* [Cabal package on hackage](http://hackage.haskell.org/package/haskell-mpi).
* [Source repository on github](http://github.com/bjpop/haskell-mpi).

    You can download the berp source repository using git with this command:

    `git clone http://github.com/bjpop/haskell-mpi.git`

#### Description

MPI is defined by the Message-Passing Interface Standard, as specified by the Message Passing Interface Forum. The latest release of the standard is known as MPI-2. These Haskell bindings are designed to work with any standards compliant implementation of MPI-2. Examples are [MPICH2](http://www.mcs.anl.gov/research/projects/mpich2) and [OpenMPI](http://www.open-mpi.org).

For an overview of using the library see the paper from issue 19 of the Monad Reader magazine [High Performance Haskell with MPI](/docs/haskell-mpi.monad.reader.pdf).

****

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

[bjpop-ray gallery](/bjpopray.html).

****

## language-python

A lexer, parser and pretty printer for Python programs, written in Haskell.

#### Downloads

* [Cabal package on hackage](http://hackage.haskell.org/package/language-python).
* [Source repository on github](http://github.com/bjpop/language-python).

    You can download the language-python source repository using git with this command:

    `git clone git://github.com/bjpop/language-python.git`

#### Description

This package provides a parser (and lexer) for Python written in Haskell. It supports version 2 and 3 of Python. The parser is implemented using the happy parser generator, and the alex lexer generator. The package also provides a pretty printer, which makes it also suitable for generating Python code.

As a demonstration of the library, I have written a simple code colouring tool which converts Python source code into XHTML files. A CSS file is used to determine the presentation of the code (such as colours). 

#### Gallery

[An example coloured Python file](/html/pycol/lsystem.py.html).

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

[An example program trace](/html/ministg/step0.html).

****

## py-lsystem

An L-System (Lindenmayer system) in Python.

#### Downloads

* [Source repository on patch tag](http://patch-tag.com/r/bjpop/py-lsystem).

    You can download the py-lsystem source repository using darcs with this command:

    `darcs get http://patch-tag.com/r/bjpop/py-lsystem/pullrepo py-lsystem`

#### Description

[L-Systems](http://en.wikipedia.org/wiki/L-system) are term rewriting languages which can have interesting interpretations in computer graphics.

Originally I wrote this program for a guest lecture in a first-year university Python course, but I have subsequently been using it to motivate computing to high-school students.

A selection of example L-System rules and the resulting images produce by py-lsystem can be seen in my L-System gallery.

#### Gallery

[L-System gallery](/lsystem.html).

****

## Scala XML parser

#### Downloads

* [Source repository on patch tag](http://patch-tag.com/r/bjpop/scala-xml-parser).

    You can download the scala-xml-parser source repository using darcs with this command:

    `darcs get http://patch-tag.com/r/bjpop/scala-xml-parser/pullrepo scala-xml-parser`

#### Description

This is a parser for a non-trivial subset of XML, written using parser combinators in Scala.

I wrote this program to demonstrate the use of parser combinators in Scala, as part of a talk I did at the Melbourne Scala Users group. See my [slides from the talk](/docs/scala_parser_combinators.pdf) for more details.

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

[Image transform gallery](/imgtrans.html).

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
