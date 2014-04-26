---
title: Fixed Points
---

First, let us ignore the lambda calculus, and just consider
functions over the natural numbers.

Given a function f, if there exists a value x, such that
x = f(x), then we say that "x is a fixed point of f". 
A function may have zero fixed points, or it may have one,
or many. In fact, a function can have infinitely many 
fixed points.

For example, this function has no fixed points:

    f(x) = x+1

This function has one fixed point:

    f(x) = 5

This function has infinitely many fixed points:

    f(x) = x

We can show by a diagonalisation argument that there are 
uncountably many functions over the natural numbers which do not
have fixed points. (See Sudkamp: Languages and Machines, 2nd
Edition, page 19). Question: what about the functions over naturals
which _do_ have fixed points. Are they countable or uncountable?

So what do fixed points have to do with computing? You can think
of a program as a finite state machine. The behaviour of the machine
is given by a "transition function", which says how the machine
goes from one state to the next. You can make the machine do work by
applying the transition function to the current state, yielding a
new state as output. Some initial state is chosen to start the
whole thing off, and a computation emerges when we iterate the 
process of applying the transition function to successive states 
of the machine.

A key question is: when do you stop? We can designate some special
state (or states) as terminals. If the terminal states are reached, 
the computation is done. But how does the designation work? Well,
we can say that the transition function becomes the identity when
given a terminal state as input. Then we can say: the computation
is finished if we reach a fixed point of the transition function.
That is: the output state is equal to the input state. Clearly the
machine will make no more progress once it has reached a fixed 
point.

Not all state transition functions have fixed points because
we can easily construct examples with loops in them.

If x is a fixed point of a function f, then we can say:

    x = FIX f

From this equation, and the general property of fixed points 
(ie. x = f(x)), we can say:

    FIX f = f (FIX f)

Thus FIX is a "fixed point finder", which takes a function 
a returns a fixed point (if one exists). Note, we don't
specify _which_ fixed point might be found, if there is more
than one. If FIX can be defined, then it must satisfy the above equation.

The natural question to ask is, can we define the fixed point
finder (FIX), and for what functions is it defined?

For functions over the natural numbers, we know that no such 
general fixed point finder exists for all functions.

Curiously, in the lambda calculus, the answer is yes, we can define
FIX, and it is defined over all terms in the lambda calculus.
What is more, FIX can be defined as a lambda term. That is
it can be defined within the lambda calculus.
This is a surprising result, and it was the cause of much 
consternation amongst mathematicians and logicians. 

Something suggests that there is a difference between terms
in the lambda calculus, and the "classical mathematical" view
of functions. And yes, there is a difference, since the
lambda calculus has a computational aspect to it, which is not
present in classical mathematics. Indeed, we already know that
the (lambda/Turing) computable functions are a proper subset
of functions over the natural numbers. 

Perhaps the most unusual thing about the lambda calculus is
that any term can be applied as if it were a function (there
is no syntactic restriction on application). Thus, 
functions in the lambda calculus are also ordinary terms
of the lambda calculus. Compare this to the classical view of
functions over natural numbers, which makes a clear distinction
between the domain of values and the functions over that domain. 
The nature of the lambda calculus can seem rather incongruous, 
especially if we have any hope of using it to
model the classical view of functions. We may return to this
topic later in semester, time permitting. 

For now, let us cover what was said in the last lecture.

I mistakenly said the following lambda term was due to
Church, but it seems it is due to Curry:

    FIX = \h -> (\x -> h (x x)) (\x -> h (x x))

We can verify that it satisfies the important fixed point 
equation by applying the function to some term f on both sides:

    FIX f = (\h -> (\x -> h (x x)) (\x -> h (x x))) f

The RHS is a beta redex, which can be reduced:

    FIX f = (\x -> f (x x)) (\x -> f (x x))

The new RHS is also a beta redex, which can be reduced:

    FIX f = f ((\x -> f (x x)) (\x -> f (x x)))

We have previously shown that the underlined term is
convertible (not reducible) to FIX f, thus we can conclude:

    FIX f = f (FIX f)

It turns out that Curry's FIX is not unique, and there are
other lambda terms which can also be used. Turing's 
version is this term:

    FIX = (\x -> \y -> y (x x y)) (\x -> \y -> y (x x y))

As an exercise, verify that Turing's term satisfies the
important equation for fixed point finders.

Let us use CURRY_FIX and TURING_FIX to name the two example
fixed point finders.

Note that both CURRY_FIX and TURING_FIX work independently of
their argument terms. That is, they are defined over all terms
in the lambda calculus. 

So every term in the lambda calculus has a fixed point!

But, this does not mean that all fixed points have a normal
form.

For instance, consider:

    CURRY_FIX (\x -> x) 

and

    TURING_FIX (\x -> x)

Neither have a normal form. And even more surprisingly, 
they don't share a common reduct. That is, they do not eventually
converge to the same reduction sequence. However, we do think of
them as denoting the same thing. What this suggests is that
reduction is only a weak form of determining equality between
terms in the lambda calculus. 

From a practical point of view, FIX gives us a way of encoding
recursion into languages (such as the lambda calculus) that
do not have it as a primitive construct. 

Let's shift into a language like Haskell for convenience of
notation.

Here is a Haskell data type encoding the natural Numbers:

    data Nat = Z | S Nat

and here is a procedure which denotes a function:

    val :: Nat -> Int
    val Z = 0
    val (S n) = 1 + val n

We can write the above procedure as one equation:

    val :: Nat -> Int
    val = \n -> case n of
                  Z -> 0
                  S m -> 1 + val m

Now, the above definition is a recursive, but we want
to write a version which is not recursive. We take
the recursive reference to "val" in the body out as a parameter.
The idea is that we want to give some meaning to the procedure
(find out the function that it denotes), but we don't want the
procedure to refer to itself, because it leads to a tangle of
self-reference.

    val_non_rec :: (Nat -> Int) -> (Nat -> Int)
    val_non_rec v = \n -> case n of
                            Z -> 0
                            S m -> 1 + v m

Notice that val_non_rec takes "v" as a parameter, and "v" is a
function of type (Nat -> Int).

We can think of val_non_rec as a "state transition function". The input
state is some approximation to the function denoted by the
val procedure, and the output state is a better approximation.

We recover the function denoted by val by
taking the fixed point of val_non_rec:

    val = fix val_non_rec

How does this compute the function?

Initially we know absolutely nothing about the function denoted by val, so
the initial state of the fixed point computation is an empty set:

    s0 = {}

We apply the state transition function (val_non_rec) to the initial state
to get the next state:

    s1 = val_non_rec s0 = \n -> case n of
                                  Z -> 0
                                  S m -> 1 + s0 m

From this step we obtain a new state:

    s1 = { (Z -> 0) }

That is by one application of the state transition function we have learned
what the function does in the base case.

We compare the new state (s1) to the old state (s0) and we see that they
are different, so we have not reached a fixed point, so we must 
continue.

    s2 = val_non_rec s1 = \n -> case n of
                                  Z -> 0
                                  S m -> 1 + s1 m

Now we can make use of the second branch in the case statement, and we
discover slightly more information about the function:

    s2 = { (Z -> 0), (S Z -> 1) }

Since (s2 != s1) we are not at a fixed point, so we continue yet again. Next 
time we discover:

    s3 = { (Z -> 0), (S Z -> 1), (S (S Z) -> 2) }

In fact, we never reach a fixed point, so this iteration goes on forever.
But that is to be expected because the function we are trying to construct is
defined over an infinite domain. Therefore the function is itself infinite.
Of course in a real program evaluation we do not need, or even want, to compute
the whole function, we just want to know what its value is for certain
elements of its domain. So in practice, we compute the fixed point on demand.
Therefore, all (Turing complete) programming languages are necessarily 
"lazy" (demand driven) to some extent.
