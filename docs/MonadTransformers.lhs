433-431 Monad transformer notes
-------------------------------

Bernie Pope.

This file is literate Haskell. Code is contained in between
"begin" and "end" delimiters.  The rest is comments.
You can load it into a Haskell interpreter. 

It is a rough transcript of the lecture from Wednesday 9 April.
Or at least, it represents what I wanted to say, even if I 
didn't use these words.

Our goal is to show that the Parser monad from previous lectures
can be reconstructed from the Maybe monad (aka failure monad),
and a state monad transformer.

First, a minor wibble, we import the Prelude and hide the 
definition of the Maybe type, then re-define it. This enables us to 
re-implement the instance of Monad for Maybe inside this module. 
It is not essential to do this, but it helps with the explanation.

\begin{code}
import Prelude hiding (Maybe (..))
data Maybe a = Nothing | Just a
\end{code}

Here is the parser monad that we know and love. Don't let the
use of "newtype" confuse you:

\begin{code}
newtype Parser a = P (String -> Maybe (String, a))

instance Monad Parser where
   return x = P (\s -> Just (s, x))
   (P f) >>= next = 
      P (\s1 -> case f s1 of
                  Nothing -> Nothing 
                  Just (s2, val) -> case next val of
                                       P g -> g s2)
\end{code}

I'm not going to explain the Parser monad because we have already
spent quite a bit of time on it. Note however, that this is not
the _only_ conceivable implementation of a parser. Other variants
are possible, but this one serves as a simple example.

Here is the conventional state monad:

\begin{code}
newtype State s a = St (s -> (s, a))

instance Monad (State s) where
   return x = St (\s -> (s, x))
   (St f) >>= next = 
      St (\s1 -> case f s1 of
                    (s2, val) -> case next val of
                                    St g -> g s2) 
\end{code}

Again, I'm not going to explain this code in detail. You should notice the
striking similarity of this code with the parser monad. The main
difference is the absence of Maybe in the state monad. Also note
that the "State" type constructor (not data constructor!) 
is parameterised by types "s" and "a". The "s" parameter 
represents the state type, the "a" parameter represents the 
type of a result computed by the state monad (ie the value yielded
by running a piece of state monad code).

The Maybe type can be used to encode "failure". The "Nothing" constructor
represents a failed computation, and the "Just" constructor represents
a successful computation, which yields a value.

Here is the instance of Monad for Maybe:

\begin{code}
instance Monad Maybe where
   return x = Just x
   Nothing >>= next = Nothing 
   Just x >>= next  = next x
\end{code}

The return function allows us to turn ordinary values into successful 
computations of those values. The first equation of >>= says that
if the left computation fails then the whole compound computation
also fails. The second equation of >>= pulls the value x out of
a successful left computation and passes it on to the rest of
the computation (which may fail or succeed - we don't know).

Lets put the Maybe monad aside for now, and pick it up again later.

We can create a slightly more general version of the 
state monad, which has an extra type parameter:

\begin{code}
newtype StateT m s a = STrans (s -> m (s, a))
\end{code}

The "m" type parameter is supposed to be some (arbitrary) 
monad, to run inside the state monad transformer. 

Now the important trick is that we can make (StateT m s)
an instance of Monad, providing that "m" is also a monad:

\begin{code}
instance Monad m => Monad (StateT m s) where
   return x = STrans (\s -> return (s, x))
   (STrans f) >>= next = 
      STrans (\s1 -> do (s2, val) <- f s1
                        case next val of
                           STrans g -> g s2)

\end{code}

Note that in the lecture I got it slightly wrong. For definition of >>= 
I had written:

" (STrans f) >>= next = 
      STrans (\s1 -> do (s2, val) <- f s1
                        STrans g <- next val 
                        g s2) "

Can you spot the error? I'll let you chew on that one. Ask me 
if you have trouble figuring out what I did wrong.

Okay, back to the correct code. The trick to understanding it is
to see that the "do" block inside the body of >>= is a computation
in the inner monad (the one corresponding to the type parameter "m").

It may help to elaborate the types. 

   (>>=) :: Monad m => StateT m s a -> (a -> StateT m s b) -> StateT m s b

Yikes! Maybe that doesn't help :)

Anyway, what is the type of "f" in the definition of >>= above?
You should be able to see that it is (look at the definition of the
StateT type again):

   f :: Monad m => s -> m (s, a)

for all types "s" and "m", and _some_ type "a". So if we apply "f" to
an "s" (the type of state) we get back:

   (f s1) :: m (s, a)

and note:

   s1 :: s

"s1" is a variable of type "s". Yes, I should have picked better names.

Ultimately, we want to see that this can be done, and that it makes sense:

   do (s2, val) <- f s1
      ...

Here we use the do notation to "run" the inner-monad computation
which results from applying "f" to the initial state.

I won't labour this point. Hopefully you can extend this line of
reasoning to figure out the types of the rest of the body of >>=.

Now, back to our goal. We want to re-build the parser monad. The 
approach that we've been working towards is this:

\begin{code}
type NewParser a = StateT Maybe String a
\end{code}

It would be nice to verify this is in fact what we want by 
specialising the Monad instance for StateT for the particular
case of NewParser. That is, in the instance Monad for StateT,
replace all occurrences of "m" with Maybe, and "s" with String.
You will need to re-write the do-block in the body of >>= to 
use the Monad instance for Maybe. It is a bit fiddly, but
you ought to be able to convince yourself that it works.

You may also recall that the Parser monad has other primitive
functions beyond ">>=" and return. For example it has the "alt"
combinator, which is written as "<|>" in Parsec.

It seems like we might have struck a problem, because,
on the one hand we are trying to generalise things with
the monad transformer, and on the other hand, we need
"special purpose" features in our monad. 

Though it is not a general solution, it just so happens that many
of the operations which seem "special purpose" are actually
instances of general purpose operations. For example, there
is a class called MonadPlus, which defines two operations:

   mzero :: MonadPlus m => m a
   mplus :: MonadPlus m => m a -> m a -> m a

In the case of parsers, mzero would be a parser which fails,
and mplus would be "<|>". 

In other monads, mplus and mzero have other meanings.
Question: would the identity monad have an instance for MonadPlus?
If so, what would it be? If not, why not?

An interesting question is how much of Parsec could be written using
only the standard monads and standard monad transformers?
I don't know what the answer to that question is. My gut feeling
is "not much", but you never know unless you try.

The final point I wanted to make was that monad transformers 
generally require some "heavy lifting", when you want to run
some computation in an inner monad. The monad transformer 
library provides a fairly general solution called lift:

   lift :: (MonadTrans t, Monad m) => m c -> t m c

This allows you to run a "m" computation inside a "t"
monad transformer computation.

If you have more than one layer in a stack of monad transformers
then you end up with nested lifts, which can get unsightly.
(There are ways around this, but they are even more unsightly.)

Below is the special case of "lifting" an IO computation into
a state monad transformer computation:

\begin{code}
liftIO :: IO a -> StateT IO s a
liftIO io = STrans (\s -> do val <- io
                             return (s, val))
\end{code}

Where to from here?

Perhaps the best introduction to the concept of monad transformers
is in the paper by Mark Jones:

   Functional Programming with Overloading and Higher-Order Polymorphism
   http://web.cecs.pdx.edu/~mpj/pubs/springschool.html
   See section 5.2 in particular.

If all of this stuff seems daunting to you, do not panic. You are not
alone. I find the best way to make progress with monads and monad
transformers is to try to use them in real code (ie terpie). 
You can treat the monads as libraries, and just program to their
interface. It is surprising how far you can get with that
approach. Eventually you will wake up one day and realise that it
all makes some sort of sense (I hope).

End of transmission.
