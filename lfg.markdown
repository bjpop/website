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
number of bits.

In Haskell the recurrence can be expressed like so:

    rec :: Integral a => a -> a -> a -> a -> a
    rec j k m = x
       where
       x n = (x (n-j) + x (n-k)) `mod` m

There is, of course, one glaring omission from this definition: base case definitions for $x_{1..k}$,
and it turns out that this is the trickiest aspect of LFGs; more on this below.

LFGs *can* have the following properties which make them a good basis for PRNGs:

   - efficient implementation (especially when the modulus is a power of 2)
   - long periods
   - good performance on standard statistical tests for randomness
   - a large number of independent streams of
     numbers can be generated from the same initial values, which is especially useful in parallel
     Monte Carlo simulations

## Period

When the modulus is $2^m$ the maximum period of an additive LFG is $(2^k - 1)2^{m-1}$, 
assuming careful choice of
the lags [4]. For example, when $k = 17$ and $m = 32$ the maximum period is $\approx 2^48$.

## Choice of lags

Coddington [6] recommends lags of at least (1063, 1279) and and preferably much larger values (for SPRNG he says "Be sure to use the largest possible lag").

## References

1. Knuth, The Art of Programming, Volume 2.
2. Mascagni *et al*, Parallel Pseudorandom number generation using additive lagged-fibonacci recursions.
3. Mascagni *et al*, A fast, high quality, and reproducible parallel lagged-fibonacci psuedorandom number generator.
4. Mascagni *et al*, SPRNG: A scalable library for psuedorandom number generation.
5. Tan, On parallel pseudo-random number generation.
6. Coddington, Random number generators for parallel computers.

****
