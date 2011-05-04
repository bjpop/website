---
title: Tools for testing RNGs
---

* [Diehard](http://www.stat.fsu.edu/pub/diehard/). Marsaglia's battery of tests for randomness. Appears
  to have been a *de facto* standard for some time, but no longer maintained. Input is a 11MB binary file
  containing a sequence of 32 unsigned integers, representing a part of the sequence to be tested. Can
  run up to 15 tests on the data. Not well documented, which makes it hard for a non-expert to understand
  the results. Disturbingly, I observed a segmentation fault on a particular test input.

* [Dieharder](http://www.phy.duke.edu/~rgb/General/dieharder.php). Compilation on OS X gives a series of 
  link errors, of the form:

         ld: duplicate symbol _splitbuf in .libs/chisq.o and .libs/bits.o

         ld: duplicate symbol _rgb_persist_rand_uint in .libs/chisq.o and .libs/bits.o

         ld: duplicate symbol _rgb_operm_k in .libs/chisq.o and .libs/bits.o

         ld: duplicate symbol _dh_rng_types in .libs/chisq.o and .libs/bits.o

         ld: duplicate symbol _gsl_types in .libs/chisq.o and .libs/bits.o

     These are caused by global variables being declared in header files without `extern` linkage. The header
     files are imported into multiple source files, which means that the symbol gets created in multiple
     object files. The solution is to declared them `extern` linkage in the header file and then define each
     global in exactly *one* source file.

* [NIST](http://csrc.nist.gov/groups/ST/toolkit/rng/index.html)

* [TestU01](http://www.iro.umontreal.ca/~simardr/testu01/tu01.html)

* [ent](http://www.fourmilab.ch/random/)

****
