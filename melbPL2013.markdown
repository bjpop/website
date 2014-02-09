---
title: Programming Languages Workshop 2013
---

## Date

Wednesday 4th December 2013, 1.15-5.15pm.

## Location

The University of Melbourne<br/>
[Doug McDonell](http://maps.unimelb.edu.au/parkville/building/168#.UpmyvqXdHLR) 503 (Level 5 Theatrette)


## Schedule

   * 1.15 - 1.30 Introduction
   * 1.30 - 2.00 [Phil Wadler, A practical theory of language-integrated query](#PhilWadler)
   * 2.00 - 2.30 [Gilad Bracha, Liveness in the Newspeak IDE](#GiladBracha)
   * 2.30 - 3.00 [Daniel Spiewak, Understanding Generalized Parsing Automata-Driven Algorithms](#DanielSpiewak)
   * 3.00 - 3.30 Intermission
   * 3.30 - 4.00 [Peter Stuckey, Modelling with Option Types](#PeterStuckey)
   * 4.00 - 4.30 [Lee Naish, Pawns: a declarative/imperative language](#LeeNaish)
   * 4.30 - 5.00 [Peter Schachte, Introducing Wybe - a language for everyone](#PeterSchachte)
   * 5.00 - 5.15 Conclusion 

## Abstracts

## <a name="PhilWadler">A practical theory of language-integrated query</a>

### Phil Wadler

Joint work with James Cheney, Sam Lindley, University of Edinburgh.

How best to blend a domain-specific language into a host language?
For the domain of databases, the old question of how to achieve
language integrated query is receiving renewed attention, in part
because of its support through Microsoft's LINQ framework.  We present
a practical theory of language-integrated query based on quotation and
normalisation of quoted terms.  Higher-order features prove useful
even for constructing first-order queries.  We prove a theorem
characterising when a host query is guaranteed to generate a single
SQL query, and we present experimental results confirming our
technique works, even in situations where Microsoft's LINQ framework
either fails to produce an SQL query or, in one case, produces an
avalanche of SQL queries.  Our ideas are implemented in F#, and the
talk briefly considers how they might apply to other languages such as
Scala and Haskell.

[Link to paper](http://homepages.inf.ed.ac.uk/wadler/topics/links.html#essence-of-linq)


****

## <a name="GiladBracha">Liveness in the Newspeak IDE</a>

### Gilad Bracha 

[Newspeak](http://newspeaklanguage.org)  is a programming language in the Smalltalk tradition which seeks to reconcile modularity, security and interoperability with reflectivity. While Smalltalk IDEs have always supported reflective update, we seek to go further and eliminate the modal distinction between editing and debugging. Whenever code is viewed, it should be tied to live program data. How to obtain this data is an interesting question: tests and prior runs are promising sources. We'll give a lightning tour of Newspeak, highlighting the issues of live programming.

****

## <a name="DanielSpiewak">Understanding Generalized Parsing Automata-Driven Algorithms</a>

### Daniel Spiewak

Parsing is a subject often treated as a solved problem, or at the very most a source of strange errors and arcane build processes.  Generalized parsing is understood by most to be similar to the aforementioned, but more…general.  This is unfortunate, as generalized parsing provides language authors with a broader toolset and a freer hand in designing the syntactic face of their language.  Working on the assumption that understanding the algorithms involved will increase adoption as well as broadly entertain, this talk will delve into the essence of generalized text parsing with a specific focus on Tomita's Graph-Structured Stack (GSS) and Scott-Johnstone's extensions to the idea.

****

## <a name="PeterStuckey">Modelling with Option Types</a>

### Peter Stuckey

Joint work with Christopher Mears, Andreas Schutt, Guido Tack, Kim Marriott, and Mark Wallace. 

Option types are a powerful modelling abstraction that allows the concise modelling of problems where some decisions are relevant only if other decisions are made. They have a wide variety of uses: for example in modelling optional tasks in scheduling, or exceptions to a usual rule. Option types represent objects which may or may not exist in the problem being modelled, and can take an ordinary value or a special value ⊤ indicating they are absent. The key property of variables of option types is that if they take the value ⊤ then the constraints they appear in should act as if the variable was not in the original definition. We explore the different ways that basic constraints can be extended to handle option types, and we show that extensions of global constraints to option types cover existing and common variants of these global constraints. We demonstrate how we have added option types to the modelling language MINIZINC. Constraints over variables with option type can either be handled by transformation into regular variables without extending the requirements on underlying solvers, or they can be passed directly to solvers that support them natively.

****

## <a name="LeeNaish">Pawns: a declarative/imperative language</a>

### Lee Naish

Pawns is yet another programming language under development which attempts
to combine the elegance of declarative programming with the algorithmic
expressive power of imperative programming.  Alegbraic data types can
be defined and viewed as descriptions of high level values which can be
manipulated in declarative ways.  The same type definitions can also be 
viewed at a much lower level, involving pointers to possibly shared data
structures which can be destructively updated.  Pawns programs contain 
annotations and declarations which tell the programmers when the low 
level view must be used.  This can allow efficient imperative code to 
be encapsulated within a declarative interface.

****

## <a name="PeterSchachte">Introducing Wybe --- a language for everyone</a>

### Peter Schachte 

We present Wybe, a new language in the early stages of development.
Wybe combines the best of declarative and imperative programming in
a principled way.  It is intended to be easy to learn for beginning
programmers, and also to scale up to large projects through good
support for software engineering principles.

****
