---
title: Programming Languages Workshop 2011
---

## Date

Tuesday 29 November 2011, 1.00-6.30pm.

## Location

The University of Melbourne<br/>
Theatre 3 (Second Floor, Room 205) ICT Building<br/>
[111 Barry Street, Carlton](http://maps.google.com.au/maps?q=111+Barry+Street,+Carlton,+Victoria&hl=en&sll=-25.335448,135.745076&sspn=41.829362,86.220703&vpsrc=1&hnear=111+Barry+St,+Carlton+Victoria+3053&t=m&z=16)<br/>


## Schedule

   * 1.00 - 1.10 Introduction
   * 1.10 - 1.40 [John Hughes, The Properties of Riak](#JohnHughes)
   * 1.40 - 2.10 [Simon Peyton Jones, Giving Haskell a Promotion](#SimonPeytonJones)
   * 2.10 - 2.40 [Peter Stuckey, Constraint Logic Programming with Learning](#PeterStuckey)
   * 2.40 - 3.10 [Andrew Bromage, Succinct Data Structures in Haskell](#AndrewBromage)
   * 3.10 - 3.30 Intermission
   * 3.30 - 4.00 [Lee Naish, Undefinedness and Underspecification](#LeeNaish)
   * 4.00 - 4.30 [Bill Birch, Meaningful Programming](#BillBirch)
   * 4.30 - 5.00 [Matt Giuca, Mars: An Imperative/Declarative Programming Language Prototype](#MattGiuca)
   * 5.00 - 5.30 [Paul Bone, Controlling Loops in Parallel Mercury Code](#PaulBone)
   * 5.30 - 6.00 [Harald S&oslash;ndergaard, Congruence Analysis with Bit-Precise Transfer Functions](#HaraldSondergaard)
   * 6.00 - 6.30 Discussion

## Abstracts

## <a name="JohnHughes">The Properties of Riak</a>

### John Hughes

Riak is one of the new breed of no-SQL database management systems, which
has begun to replace relational databases for some applications. Riak is a
distributed key-value store, inspired by Amazon's Dynamo, designed for
applications where scalability, low latency and high availability are
critical. Riak uses replication to provide fast access to data, even when
multiple nodes or parts of the network fail. It supports concurrent access
to the same data by multiple clients, even when the network is partitioned.
All of this makes it very hard to test.

I will show how QuickCheck helped us to model Riak's behavior, improving
understanding and revealing the occasional bug.

****

## <a name="SimonPeytonJones">Giving Haskell a Promotion</a>

### Simon Peyton Jones

Static type systems strive to be richly expressive while still being simple
enough for programmers to use. We describe an experiment that enriches Haskell's
kind system with two features promoted from its type system: data types and polymorphism.
The new system has a very good power-to-weight ratio: it offers a significant
improvement in expressiveness, but, by re-using concepts that programmers are already
familiar with, the system is easy to understand and implement.

****

## <a name="PeterStuckey">Constraint Logic Programming with Learning</a>

### Peter Stuckey

Constraint logic programming (CLP) is the archetypal constraint programming
language (scheme). It allows the specification of constraint problems as
well as search strategies in a clear and simple manner. 
But most modern constraint programming systems are not CLP systems,
instead they allow a fixed set of constraints to be specified, and search to
be controlled using a different language. 
Recently CP systems have been substantially improved by the addition of
*learning* techniques. Learning records sets of decisions that do not
lead to a solution and avoid repeating the same set of decisions.
In this talk we extend learning techniques to CLP systems,
which allows the same search benefits to be derived for more complex
kinds of hierarchical search.

****

## <a name="AndrewBromage">Succinct Data Structures in Haskell</a>

### Andrew Bromage

Data structure compression differs from traditional data compression in that
the compressed data does not need to be decompressed to do useful work
with it. Moreover, many common data structures can be implemented in space
which is very close to optimal. This talk is a brief introduction to the field, with
example code in Haskell.

****

## <a name="LeeNaish">Undefinedness and Underspecification</a>

### Lee Naish

At the core of programming is the relationship between what is computed
and what we intend to compute.  To describe precisely what is computed
by any reasonably expressive system, some notion of undefinedness is
essential.  We have known there are limits to what can be formally defined
and computed since the work of G&ouml;del and Turing.  To describe precisely
what we intend to compute, some notion of underspecification is essential
to express concepts such as "garbage in" and "pre-conditions".  This has
been known since the time of Babbage.  Drawing on recent work in logic
programming, we argue that undefinedness is the dual of underspecification
and having a framework which incorporates both concepts is desirable.

****

## <a name="BillBirch">Meaningful Programming</a>

### Bill Birch

Evolving programming languages to integrate with the Semantic Web and
its underlying concepts. We will start with Lisp 1 and describe what
happens when SemWeb ideas are merged with it.  What could be the
benefits? What's the implication of a type system based on Description
Logics?

****

## <a name="MattGiuca">Mars: An Imperative/Declarative Programming Language Prototype</a>

### Matt Giuca

For years, we have enjoyed the robustness of programming without side-effects in
languages such as Haskell and Mercury. Yet in order to use languages with true
referential transparency, we seem to have to give up basic imperative constructs
that we have become accustomed to: looping constructs, variable update, destructive
update of arrays, and so on. In my programming language, [Mars](http://ww2.cs.mu.oz.au/~mgiuca/mars/),
the programmer is allowed
to write imperative-style code using Python syntax, yet has the benefits of a language
with referential transparency. An unreleased version of Mars performs analysis to
automatically perform destructive update of arrays wherever possible.

I will give a demo of this prototype language, and show some of the new features
since my previous talk, including the automatic destructive update. I also discuss
the design philosophy behind the language, and its future directions.

****

## <a name="PaulBone">Controlling Loops in Parallel Mercury Code</a>

### Paul Bone

Recently we built a system that uses profiling data to automatically
parallelize Mercury programs by finding conjunctions with expensive conjuncts
that can run in parallel with minimal synchronization delays.  This worked very
well in many cases, but in cases of tail recursion, we got substantially lower
speedups than we expected, due to excessive memory usage.  In this paper, we
present a novel program transformation that eliminates this problem, and for
the first time allows recursive calls inside parallel conjunctions to take
advantage of tail recursion optimization.  Our benchmark results show that our
new transformation greatly increases the speedups we can get from parallel
Mercury programs.  In some cases, it doubles the speedup, and in one case, it
increases it by almost a factor of four, from no speedup to almost perfect
speedup on four cores.

****

## <a name="HaraldSondergaard">Congruence Analysis with Bit-Precise Transfer Functions</a>

### Harald S&oslash;ndergaard

The talk is about analysis and verification of programs that use
bit-twiddling for numerical computation over fixed-width integers.
Using a technique first proposed by Reps, Sagiv and Yorsh, we show
how local, bit-precise analysis can be used to enhance congruence
analysis.  Escaping the usual restriction to linear assignments,
such analysis enables automated verification of bit-manipulating
programs, without necessarily incurring excessive cost.  The work
presented has been done jointly with Andy King.

****
