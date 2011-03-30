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

1. Install gcc 4.3 from macports:

         sudo port install gcc43

    In addition to a C compiler, this will also provide C++ (g++) and Fortran (gfortran) compilers. Xcode does not ship with Fortran. Normally this will put the compilers in:

         /opt/local/bin/gcc-mp-4.3
         /opt/local/bin/g++-mp-4.3
         /opt/local/bin/gfortran-mp-4.3

2. Untar the SPRNG source, and move to the top-level of the untarred directory:

         tar xvf sprng4.tar
         cd sprng4

3. Apply [this patch file](/files/sprng.patch.txt) to the source code:

         patch -p1 < sprng.patch.txt

    For the most part this patch fixes up some deprecated `#include` statements,
    such as `#include <iostream.h>` which should be `#include <iostream>`. It also includes some
    libraries, such as `cstdlib` and adds the `std` namespace in some places.

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

4. Configure the code:

        ./configure FFLAGS=-fsecond-underscore \
        CXX=/opt/local/bin/g++-mp-4.3 \
        F77=/opt/local/bin/gfortran-mp-4.3 \
        MPICXX=/opt/local/bin/g++-mp-4.3 \
        MPIF77=/opt/local/bin/gfortran-mp-4.3

    The `-fsecond-underscore` tells the `gfortran` compiler to add an extra underscore at the end of symbols as part of its name mangling scheme. Note that we set both `CXX` and `F77` to point to the same things as `MPICXX` and `MPIF77` repsectively. This is a little hack to convince the build system not to try to build the MPI version of the library. See the MPI notes below about why this is done.

    I think the default is to try to install to `/usr/local/`. This is probably a bad place on OS X, and you should think about putting it somewhere else. To pick another location, use the `--prefix=` option to `./configure`. For example, amend the previous `./configure` line to be something like:

        ./configure --prefix=/my/install/path/ ...

5. Build and install the code:

        make
        make install
