---
title: Lagged Fibonacci PRNGs
---

# Lagged Fibonacci psuedorandom number generators

## Introduction

The recurrence relation for simple LFGs is:

$x_n = x_{n-j} \odot x_{n-k}\ (mod\ m),\ 0 < j < k$

where $\odot$ is some binary operator, typically one of: $+ - \times \oplus$. When
the operator is $+$ the generator is *additive*; when it is $\times$ the generator is
*multiplicative*. The indices $j,k$ are the *lags* of the generator. The modulus $m$
reflects the fact that individual numbers in the sequence are stored in a fixed
number of bits. For generating sequences of floating point numbers $m$ 
is 1 and $x_n \in [0,1)$.

In Haskell the integral recurrence can be expressed like so:

    rec j k m = x
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
common to use a modulus which is a power of two, $m = 2^p$. In this case
the maximum period of an additive LFG is $(2^k - 1)2^{p-1}$,
assuming careful choice of
the lags and initial sequence (seed) [4].
For example, when $k = 1279$ and $m = 32$ the maximum period is $\approx 2^1310$.

## Initialisation

An LFG must be initialised by the first $k$ elements of the sequence. The choice of initial values
is signficant for the quality of the resulting sequence. For instance, if all the initial values are even,
then *all* the numbers in the rest of the generated sequence will be even too (because the sum of two evens
is even). So at least one of the initial values must be odd. A more complex issue is that the set of all
initialisers can be split into equivalence classes. Each member of a particular equivalence class will
produce the same periodic sequence of numbers (but perhaps starting at a different point). The number
of equivalence classes is $2^{(k-1)(p-1)}$. This allows us to generate a large number of independent
sequences of numbers, which can be useful in parallel computations because, once the sequences are
initialised, there is no need for data exchange between processors. It should be noted that, while the
number of equivalence classes is large, it is still finite. It is obviously undesirable to generate all
the indepenent sequences up front, so some method of "spawning" must be provided. That is to say, we must
be able to  generate a
new sequence from an existing one such that the two are from different equivalence classes. For
parallel applications, some kind of numbering scheme is required to avoid inadvertently
spawning the same sequence on different processors.
Initialisation of LFGs presents two challenges:

1. Finding canonical forms for the initialisers of each equivalence class.
2. Enumerating the different equivalence classes.

The solution to these two problems is the subject of some complicated work discussed in various
papers by Mascagni et al, and the source of considerable complexity in the source code of the SPRNG library.

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

****
