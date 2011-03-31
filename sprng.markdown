---
title: Building the SPRNG library
---

## Background

[SPRNG (Scalable Parallel Pseudo Random Number Generators Library)](http://sprng.cs.fsu.edu/) is
a library of pseudo random number generators, provided by the Department of Computer Science and the School of Computational Science at Florida State University.

At the time of writing this document (March 2011), the latest version of SPRNG is 4, which was released in
July 2007. The source is a mixture of C++ and Fortran, and can be compiled with or without MPI.
The SPRNG website suggests that you can build the code like so:

    tar xvf sprng4.tar
    ./configure
    make

but this did not work for me on OS X Snow Leopard (10.6.7) using recent versions of GCC (>= 4.3).

In this note below I describe the steps I took to build the (non-MPI) version of the code on my mac. I expect
similar steps will apply to other unix environments such as Linux.

## Instructions

1. Install gcc 4.4. This will provide C (gcc), C++ (g++) and Fortran (gfortran) compilers.
   Xcode does not ship with Fortran.On OS X you can install it from macports like so:

         sudo port install gcc44

    Normally this will put the compilers in:

         /opt/local/bin/gcc-mp-4.4
         /opt/local/bin/g++-mp-4.4
         /opt/local/bin/gfortran-mp-4.4

2. Untar the SPRNG source, and move to the top-level of the untarred directory:

         tar xvf sprng4.tar
         cd sprng4

3. Apply [this patch file](/files/sprng.patch.txt) to the source code:

         patch -p1 < sprng.patch.txt

    The patch fixes the following problems:

       - deprecated header includes of `<iostream.h>` and `<iomanip.h>`, which should
         be `<iostream>` and `<iomanip>`.
       - addition of `using namespace std;` where needed.
       - inclusion of `<cstdlib>` and `<cstring>` where needed.
       - fix of `#ifdef SYNC ... #elsif !SYNC` which causes trouble for gcc >= 4.4.

    If the patch works correctly you should see this output after running the above command:

        patching file SRC/bignum.cpp
        patching file SRC/bignum.h
        patching file SRC/cmrg/cmrg.cpp
        patching file SRC/fwrap.cpp
        patching file SRC/fwrap.cpp~
        patching file SRC/lcg/lcg.cpp
        patching file SRC/lcg64/lcg64.cpp
        patching file SRC/lfg/lfg.cpp
        patching file SRC/mlfg/mlfg.cpp
        patching file SRC/pmlcg/bignum.h
        patching file SRC/pmlcg/pmlcg.cpp
        patching file SRC/pmlcg/pmlcg.cpp~
        patching file SRC/sprng_cpp.cpp
        patching file TESTS/chisquare.cpp
        patching file TESTS/coupon.cpp
        patching file TESTS/metropolis.cpp
        patching file TESTS/mpitests/coupon.cpp
        patching file EXAMPLES/mpisprng/message_mpi.cpp
        patching file EXAMPLES/mpisprng/seed_mpi.cpp
        patching file EXAMPLES/mpisprng/sprng_mpi.cpp
        patching file EXAMPLES/pi-simple.cpp
        patching file EXAMPLES/spawn.cpp
        patching file include/bignum.h
        patching file include/bignum.h.copy

4. Configure the code:

        ./configure FFLAGS=-fsecond-underscore \
        CXX=/opt/local/bin/g++-mp-4.4 \
        F77=/opt/local/bin/gfortran-mp-4.4 \
        MPICXX=/opt/local/bin/g++-mp-4.4 \
        MPIF77=/opt/local/bin/gfortran-mp-4.4

    The `-fsecond-underscore` tells the `gfortran` compiler to add an extra underscore at the end of symbols as part of its name mangling scheme. The code doesn't seem to link properly without this option. Note that we set both `CXX` and `F77` to point to the same things as `MPICXX` and `MPIF77` repsectively. This is a little hack to convince the build system not to try to build the MPI version of the library. See the MPI notes below about why this is done.

    I think the default is to try to install to `/usr/local/`. This is probably a bad place on OS X, and you should think about putting it somewhere else. To pick another location, use the `--prefix=` option to `./configure`. For example, amend the previous `./configure` line to be something like:

        ./configure --prefix=/my/install/path/ ...

5. Build and install the code:

        make
        make install

## Remaining problems

Here are some problems which remain unsolved:

1. The SPRNG site suggests that you can time the various random number generators by running the command:

        check/timesprng

    When I do this for versions built with gcc 4.3, it crashes with:

        pmlcg: Timing FORTRAN interface:
        terminate called after throwing an instance of 'std::bad_alloc'
        what():  std::bad_alloc
        ./timesprng: line 36: 98557 Abort trap              ././pmlcg/time.fpmlcg

    The same problem does not appear to happen when SPRNG is built with gcc 4.4.

2. SPRNG comes with some support for distributed parallel programming using the MPI interface.
   By default the build system for SPRNG will try to compile the MPI version, but on my machine
   the build fails like so:

        Undefined symbols:
        "std::ctype<char>::_M_widen_init() const", referenced from:
        mult_48_32(int*, int*, int*)in libsprng.a(libsprng_a-lcg.o)
        operator<<(std::basic_ostream<char, std::char_traits<char> >&, BigNum const&)in libsprng.a(libsprng_a-bignum.o)
        operator<<(std::basic_ostream<char, std::char_traits<char> >&, BigNum const&)in libsprng.a(libsprng_a-bignum.o)

    This appears to be a bug in g++ 4.4 regarding inlining of `std::endl`. Problem does not
    appear to be present in gcc 4.3. However, if you build with gcc 4.3 then you get the bug mentioned above
    and the MPI build fails at a later stage when building the test cases.

    In the end I decided I don't really need the MPI support, so I didn't bother investigating further.

    Note: there didn't seem to be an obvious way to stop the default build of SPRNG from
    using MPI. Looking at `configure.ac` we see this test:

        AC_CHECK_PROGS(MPICXX, mpic++ mpicxx mpiCC hcp mpxlC_r mpxlC mpCC cmpic++, $CXX)
        AC_CHECK_PROGS(MPIF77, mpif77 hf77 mpxlf_r mpxlf mpf77 cmpifc, $F77)

        if test $CXX != $MPICXX; then--
           can_use_mpi=y
        else
           can_use_mpi=n
        fi

        AM_CONDITIONAL(USE_MPI, test $can_use_mpi = y)

        if test $can_use_mpi = y; then
           MPI_DEF="-DSPRNG_MPI"
        else
           MPI_DEF=""
        fi

    So we can see that by setting `$CXX` to equal `$MPICXX` we will cause the MPI version to not be built.
    This explains my `./configure` line above which sets them both to `/opt/local/bin/g++-mp-4.4`.
