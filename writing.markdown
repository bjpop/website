---
title: Writing
---

## Theses

* PhD thesis: [A Declarative Debugger for Haskell](/docs/BerniePope.PhD.Thesis.pdf).

    This thesis is about the design and implementation of a debugging tool which helps Haskell programmers understand why their programs do not work as intended. The traditional debugging technique of examining the program execution step-by-step, popular with imperative languages, is less suitable for Haskell because its unorthodox evaluation strategy is difficult to relate to the structure of the original program source code. We build a debugger which focuses on the high-level logical meaning of a program rather than its evaluation order. This style of debugging is called declarative debugging, and it originated in logic programming languages. At the heart of the debugger is a tree which records information about the evaluation of the program in a manner which is easy to relate to the structure of the program. Links between nodes in the tree reflect logical relationships between entities in the source code. An error diagnosis algorithm is applied to the tree in a top-down fashion, searching for causes of bugs. The search is guided by an oracle, who knows how each part of the program should behave. The oracle is normally a human - typically the person who wrote the program - however, much of its behaviour can be encoded in software.

    Award: [Best PhD Thesis](http://core.edu.au/index.php/page/award-rec) from the [The Computing Research and Education Association of Australasia (CORE)](http://core.edu.au).

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

    In this article, we give a brief overview of the Haskell-MPI library and show how it can be used to write distributed parallel programs. We use the trapezoid method for approximating definite integrals as a motivating example and compare the performance of an implementation using Haskell-MPI to three variations of the same algorithm: a sequential Haskell program, a multi-threaded Haskell program, and a C program also using MPI.

* [Performance of Hybrid Programming Models for Multiscale Cardiac Simulations: Preparing for Petascale Computation](http://ieeexplore.ieee.org/xpls/icp.jsp?arnumber=5951744). IEEE Transactions on Biomedical Engineering. Volume 58, Issue 10, October 2011. B.J. Pope, B.G. Fitch, M.C. Pitman, J.J. Rice, M. Reumann.

    Future multiscale and multiphysics models that support research into human disease, translational medical science, and treatment can utilize the power of high-performance computing (HPC) systems. We anticipate that computationally efficient multiscale models will require the use of sophisticated hybrid programming models, mixing distributed message-passing processes [e.g., the message-passing interface (MPI)] with multithreading (e.g., OpenMP, Pthreads). The objective of this study is to compare the performance of such hybrid programming models when applied to the simulation of a realistic physiological multiscale model of the heart. Our results show that the hybrid models perform favorably when compared to an implementation using only the MPI and, furthermore, that OpenMP in combination with the MPI provides a satisfactory compromise between performance and code complexity. Having the ability to use threads within MPI processes enables the sophisticated use of all processor cores for both computation and communication phases. Considering that HPC systems in 2012 will have two orders of magnitude more cores than what was used in this study, we believe that faster than real-time multiscale cardiac simulations can be achieved on these systems.

* [Petascale computation performance of lightweight multiscale cardiac models using Hybrid Programming Models](http://www.ncbi.nlm.nih.gov/pubmed/22254341). Conference Proceedings of the IEEE Engingeering in Medicine and Biology Society. 2011 pages 433-6. B.J. Pope, B.G. Fitch, M.C. Pitman, J.J. Rice, M. Reumann.  

    Future multiscale and multiphysics models must use the power of high performance computing (HPC) systems to enable research into human disease, translational medical science, and treatment. Previously we showed that computationally efficient multiscale models will require the use of sophisticated hybrid programming models, mixing distributed message passing processes (e.g. the message passing interface (MPI)) with multithreading (e.g. OpenMP, POSIX pthreads). The objective of this work is to compare the performance of such hybrid programming models when applied to the simulation of a lightweight multiscale cardiac model. Our results show that the hybrid models do not perform favourably when compared to an implementation using only MPI which is in contrast to our results using complex physiological models. Thus, with regards to lightweight multiscale cardiac models, the user may not need to increase programming complexity by using a hybrid programming approach. However, considering that model complexity will increase as well as the HPC system size in both node count and number of cores per node, it is still foreseeable that we will achieve faster than real time multiscale cardiac simulations on these systems using hybrid programming models. 

* [Rare Mutations in XRCC2 Increase the Risk of Breast Cancer](http://www.sciencedirect.com/science/article/pii/S0002929712001450). American Journal of Human Genetics, Volume 90, Issue 4, 6 April 2012, Pages 734–739. D.J. Park, F. Lesueur, T. Nguyen-Dumont, M. Pertesi, F. Odefrey, F. Hammet, S.L. Neuhausen, E.M. John, I.L. Andrulis, M.B. Terry, M. Daly, S. Buys, F. Le Calvez-Kelm, A. Lonie, B.J. Pope, H. Tsimiklis, C. Voegele, F.M. Hilbers, N. Hoogerbrugge, A. Barroso, A. Osorio, the Breast Cancer Family Registry the Kathleen Cuningham Foundation Consortium for Research into Familial Breast Cancer G.G. Giles, P. Devilee, J. Benitez, J.L. Hopper, S.V. Tavtigian, D.E. Goldgar, M.C. Southey. 

    An exome-sequencing study of families with multiple breast-cancer-affected individuals identified two families with XRCC2 mutations, one with a protein-truncating mutation and one with a probably deleterious missense mutation. We performed a population-based case-control mutation-screening study that identified six probably pathogenic coding variants in 1,308 cases with early-onset breast cancer and no variants in 1,120 controls (the severity grading was p < 0.02). We also performed additional mutation screening in 689 multiple-case families. We identified ten breast-cancer-affected families with protein-truncating or probably deleterious rare missense variants in XRCC2. Our identification of XRCC2 as a breast cancer susceptibility gene thus increases the proportion of breast cancers that are associated with homologous recombination-DNA-repair dysfunction and Fanconi anemia and could therefore benefit from specific targeted treatments such as PARP (poly ADP ribose polymerase) inhibitors. This study demonstrates the power of massively parallel sequencing for discovering susceptibility genes for common, complex diseases.

* [Bpipe: a tool for running and managing bioinformatics pipelines](http://bioinformatics.oxfordjournals.org/content/28/11/1525). Bioinformatics, Volume 28, Issue 11, 2012 pages 1525-1526. Simon P. Sadedin, Bernard Pope and Alicia Oshlack.

    Bpipe is a simple, dedicated programming language for defining and executing bioinformatics pipelines. It specializes in enabling users to turn existing pipelines based on shell scripts or command line tools into highly flexible, adaptable and maintainable workflows with a minimum of effort. Bpipe ensures that pipelines execute in a controlled and repeatable fashion and keeps audit trails and logs to ensure that experimental results are reproducible. Requiring only Java as a dependency, Bpipe is fully self-contained and cross-platform, making it very easy to adopt and deploy into existing environments.
