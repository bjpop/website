---
title: Lagged Fibonacci PRNGs
---

# Lagged Fibonacci psuedorandom number generators

## Introduction

The recurrence relation for simple LFGs is:

$x_n = x_{n-j} \odot x_{n-k}\ (mod\ m),\ 0 < j < k$

where $\odot$ is some binary operator, typically one of: $+ - \times \oplus$. When
the operator is $+$ the generator is *additive* (ALFG); when it is $\times$ the generator is
*multiplicative* (MLFG). The indices $j,k$ are the *lags* of the generator. The modulus $m$
reflects the fact that individual numbers in the sequence are stored in a fixed
number of bits. For generating sequences of floating point numbers $m$
is 1 and $x_n \in [0,1)$.

In Haskell the ALFG recurrence can be expressed like so:

    alfg j k m = x
       where
       x n = (x (n-j) + x (n-k)) `mod` m

There is, of course, one glaring omission from this definition: base case definitions for $x_{1..k}$,
and it turns out that initialisation (seeding) is the trickiest aspect of LFGs; more on this below.

LFGs *can* have the following properties which make them a good basis for PRNGs:

   - Efficient implementation.
   - Long periods.
   - Good performance on standard statistical tests for randomness.
   - A large number of independent streams of
     numbers can be generated from the same initial values.

Typical for recurrences of this form, an efficient implementation is based on tabulating the last $k$ values
in the sequence. Thus $O(k)$ words of state are needed.

## Period

When the modulus is prime a maximum period of $m^k - 1$ is possible [3]. However, for
efficiency reasons, it is more
common to use a modulus which is a power of two, $m = 2^p$. In this case, the maximum periods are:

   - ALFG: $(2^k - 1)2^{p-1}$
   - MLFG: $(2^k - 1)2^{p-3}$

To attain these maximum periods we must be careful with our choice of lags and initial sequence (seed) [4].
For example, for the ALFG case, when $k = 1279$ and $m = 32$ the maximum period is $\approx 2^1310$.

A longer period is important for applications which need a large number of pseudo random numbers, and
clearly the ALFG is superior to the MLFG in this regard. However, it has been found that the MFLG has better
randomness properties than the ALFG. Despite this fact, the ALFGs have tended to be more popular because
addition is traditionally faster then multiplication on general purpose hardware (however, this
property has come into question more recently [9]).

## Initialisation (seeding)

An LFG must be initialised by the first $k$ elements of the sequence. Some authors refer to this initial
sequence as the *seed* of the generator. This terminology is different than the traditional
meaning of "seed" for PRNGS, which is simply an offset into the sequence; a starting position.
The choice of initial values
is signficant for the quality of the resulting sequence. For instance, if all the initial values are even in
an ALFG,
then *all* the numbers in the rest of the generated sequence will be even too (because the sum of two evens
is even). So at least one of the initial values must be odd. The papers by Mascagni describe a complex
initialisation process which is designed to facilitate spawning of new independent sequences from an
initial sequence (see below). An simpler alternative approach is adopted by [9], where they intialise
the sequence using another PRNG (Mersenne Twister, MT19937).

## Spawning

A more complex issue is that the set of all
initialisers can be split into equivalence classes. Each member of a particular equivalence class will
produce the same periodic sequence of numbers (but perhaps starting at a different point). For the
ALFG, the number
of equivalence classes is $2^{(k-1)(p-1)}$. This allows us to generate a large number of independent
sequences of numbers, which can be useful in parallel computations.
It should be noted that, while the
number of equivalence classes is large, it is still finite. It is obviously undesirable to generate all
the indepenent sequences up front, so some method of "spawning" must be provided. That is to say, we must
be able to  generate a
new sequence from an existing one such that the two are from different equivalence classes. For
parallel applications, some kind of numbering scheme is required to avoid inadvertently
spawning the same sequence on different processors, see [14].
Initialisation of LFGs presents two challenges:

1. Finding canonical forms for the initialisers of each equivalence class.
2. Enumerating the different equivalence classes.

The solution to these two problems is the subject of some complicated work discussed in various
papers by Mascagni et al, and the source of considerable complexity in the source code of the SPRNG library.
Note that [9] suggests that splitting on equivalence classes is uneccessary, and good parallel generators
can be achieved by seeding with another PRNG with good statistical properties, such as the Mersenne Twister.

## Choice of lags

Coddington [6] recommends lags of at least (1063, 1279) and and preferably much larger values (for SPRNG he says "Be sure to use the largest possible lag").

## References

1. Knuth, The Art of Programming, Volume 2.
2. Mascagni *et al*, Parallel Pseudorandom number generation using additive lagged-fibonacci recursions.
3. Mascagni *et al*, A fast, high quality, and reproducible parallel lagged-fibonacci psuedorandom number generator.
4. Mascagni *et al*, SPRNG: A scalable library for psuedorandom number generation.
5. Tan, On parallel pseudo-random number generation.
6. Coddington, Random number generators for parallel computers.
7. Brent, Uniform random number generators for supercomputers.
8. Brent, On the periods of generalized Fibonacci recurrences.
9. Tan and Blais, PLFG: A Highly Scalable Parallel Pseudo-random Number Generator for Monte Carlo Simulations.
10. Orue et al, Trifork, a New Pseudorandom Number Generator Based on Lagged Fibonacci Maps.
11. Marsaglia, Matrices and the structure of random number sequences, Linear Algebra and its Applications, Volume 67, June 1985, Pages 147-156.
12. Makino, Lagged-Fibonacci random number generators on parallel computers, Parallel Computing, Volume 20, Issue 9, September 1994, Pages 1357-1367.
13. Matsumoto et al, Common defects in initialization of pseudorandom number generators, ACM Transactions on Modeling and Computer Simulation, Volume 17 Issue 4, September 2007.
14. Pryor et al, Implementation of a portable and reproducible parallel pseudorandom number generator, Proceedings Supercomputing 94.



****
