<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Why MatLisp(and Lisp)?</a>
<ul>
<li><a href="#sec-1-1">1.1. How does this compare to Matlab, SciPy, Octave, R etc?</a></li>
</ul>
</li>
<li><a href="#sec-2">2. How to Install</a></li>
<li><a href="#sec-3">3. Example usage</a></li>
<li><a href="#sec-4">4. Progress Tracker</a>
<ul>
<li><a href="#sec-4-1">4.1. What works ?</a></li>
<li><a href="#sec-4-2">4.2. <span class="todo TODO">TODO</span> : What remains ? (Help!)</a>
<ul>
<li><a href="#sec-4-2-1">4.2.1. Get rid of the build system</a></li>
<li><a href="#sec-4-2-2">4.2.2. Unify slicing syntax</a></li>
<li><a href="#sec-4-2-3">4.2.3. Functionality</a></li>
<li><a href="#sec-4-2-4">4.2.4. Gnuplot interface</a></li>
<li><a href="#sec-4-2-5">4.2.5. Python-bridge</a></li>
<li><a href="#sec-4-2-6">4.2.6. Support linking to libraries ?</a></li>
<li><a href="#sec-4-2-7">4.2.7. Documentation, tests</a></li>
<li><a href="#sec-4-2-8">4.2.8. Symbolics, AD, more fancy stuff {wishlist}</a></li>
</ul>
</li>
</ul>
</li>
</ul>
</div>
</div>


MatLisp is intended to be a base for scientific computation in Lisp.
This is the development branch of Matlisp.

MatLisp is made from a mixture of CLOS, lots of macros and a healthy disdain for
other programming languages :) The old version of Matlisp was aimed mostly towards
handling matrices: being a Lispy-interface to BLAS/LAPACK amongst other things.

The version in development is intended for more general usage. The current version can handle
general dense tensors, sparse matrices, and more!

It generates and compiles type-specialized generic methods on the fly, and uses lots of macros
to simplify writing very tight loops.

# Why MatLisp(and Lisp)?<a id="sec-1" name="sec-1"></a>

Lisp is a very hacker friendly language - the difference between
source and binary is almost non-existent. With Lisp, you have at your disposable
the complete Lisp language, with CLOS, macros, and tons of libraries.
Using compilers such as SBCL, and macros.

## How does this compare to Matlab, SciPy, Octave, R etc?<a id="sec-1-1" name="sec-1-1"></a>

These packages are in a much more mature stage than Matlisp. This also implies
that we lack a lot of the features which come from having a thriving ecosystem.
However, Matlisp was built with the hope that expressibility and performance don't
necessarily have to mutually exclusive; in that we're not alone: Lush, and Julia
both have similar goals.

The usual scientific environment is an amalgamation of a slow, interpreted, dynamically
typed language with tons of libraries and sugary interfaces to BLAS/LAPACK.

On the other hand, Common Lisp, is compiled + interpreted, dynamically typed + optional type
declarations + type inferencing, and comes with an awesome development environment: SLIME.
It, of course, suffers from the ultimate sin, not being popular.

With all things equal(which they are not), Matlisp(+SBCL) beats or is about as
fast as any of these packages; if you really want to squeeze every bit of
computational power, you can in most cases optimize loops, so that they're
as fast as C. However, there are limitations to what you can do vs C, because
of the Lisp implementation overheads (things like SSE, blocked computation).

For instance,
> (defun mm (A B C)
    (einstein-sum real-tensor (j k i) (ref C i j) (\* (ref A i j) (ref B j k))))
basically generates an extremely tight naive 3-loop version of GEMM (can you figure
out the reason for the odd loop-index ordering ?), which is only about 10 times as
slow the optimized version of GEMM in OpenBLAS (which does blocked computation).
The performance with SBCL for the above is within 10-15% of the corresponding C
code.

# How to Install<a id="sec-2" name="sec-2"></a>

Matlisp uses CFFI for callng foreign functions. That said, the FFI
capabilities of different lisps are quite different. So if your Lisp
implementation supports callbacks and calling plain C functions and
maps float simple-arrays into C-type arrays in memory, it shouldn't
be too hard to get it working (if it doesn't work already). We've tested
Matlisp on CCL and SBCL. The build system is cranky with all the new changes.

Linux/Unix Installation:
`======================`

One of the design goals of Matlisp was to ensure the consistency of
installation.  Matlisp is currently distributed as source code and the
user must do a compilation.  A great deal of effort was put into a
configure script that determines machine parameters, system libraries
and without bothering the user.

The installation follows in a few easy steps:

Download and install quicklisp <http://www.quicklisp.org/beta/>; make sure the quicklisp directory is "~/quicklisp/"
(This step makes sure that CFFI is available, more advanced users can skip this and install
CFFI and make it visible to ASDF).

Download the Matlisp:
> git clone git://git.code.sf.net/p/matlisp/git matlisp-git
> cd matlisp-git
> git checkout tensor

Install all the configuration scripts the first time.
> autoreconf &#x2013;install

Create a build directory.  (You can use any name you like).
> mkdir build
> cd build

Use the following if you want to build and use the reference blas/lapack implementation.
> ../configure &#x2013;libdir=$PWD/lib &#x2013;enable-static=no &#x2013;enable-<lisp> &#x2013;with-lisp-exec=<exec>
(<lisp> &isin; {sbcl, ccl, cmucl, acl})
You don't need to build matlisp in order to use a different lisp implementations (yes, this
is quite redundant).

If you already have a optimized version of BLAS/LAPACK:
> ../configure &#x2013;libdir=$PWD/lib &#x2013;enable-static=no &#x2013;enable-<lisp> &#x2013;with-lisp-exec=<exec> &#x2013;with-external-blas-lapack=<path>
On linux, <path> is usually *usr/lib*
On Mac OSX, you can use vecLib by setting <path> to *System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/Versions/A*

If configure does not select the desired Fortran compiler and
compiler flags, you can specify them like this:
../configure F77=f77 FFLAGS='-g -O -KPIC' &#x2026;

Avanti!
> make
This should land you in a lisp shell at the end.

To use matlisp after building just call
CL-USER> (load "<matlisp-path>/build/start.lisp")
CL-USER> (in-package :matlisp)

# Example usage<a id="sec-3" name="sec-3"></a>

More documentation will be added as things reach a nicer stage of development.

    ;;Creation
    MATLISP> (copy! (randn '(2 2)) (zeros '(2 2) 'complex-tensor))
    #<COMPLEX-TENSOR #(2 2)
    -1.5330     -1.67578E-2
    -.62578     -.63278
    >
    
    ;;gemv
    MATLISP> (let ((a (randn '(2 2)))
                   (b (randn 2)))
               (gemv 1 a b nil nil))
    #<REAL-TENSOR #(2)
    1.1885     0.95746
    >
    
    ;;Tensor contraction
    MATLISP> (let ((H (randn '(2 2 2)))
                   (b (randn 2))
                   (c (randn 2))
                   (f (zeros 2)))
               (einstein-sum real-tensor (i j k) (ref f i) (* (ref H i j k) (ref b j) (ref c k))))
    #<REAL-TENSOR #(2)
    0.62586     -1.1128
    >

# Progress Tracker<a id="sec-4" name="sec-4"></a>

## What works ?<a id="sec-4-1" name="sec-4-1"></a>

-   Generic template structure.
-   Double real, complex tensor structures in place.
-   Templates for optimized BLAS methods in Lisp.
-   Automatic switching between Lisp routines and BLAS.
-   Inplace slicing, real - imag views for complex tensors.
-   copy, scal, dot, swap, axpy, gemv, gemm, getrf/getrs (lu), geev(eig), potrf/potrs(chol), geqr
-   permutation class, sorting, conversion between action and
    cycle representations.
-   mod-loop works, can produce very quick multi-index loops.
-   einstein macro works, can produce optimized loops.

## TODO : What remains ? (Help!)<a id="sec-4-2" name="sec-4-2"></a>

### Get rid of the build system<a id="sec-4-2-1" name="sec-4-2-1"></a>

Use cffi:foreign-symbol-pointer and things to check for Fortran name mangling convention; and move things
requiring a fortran compiler to their own packages. Matlisp can then be made available on Quicklisp. BLAS/LAPACK
code will obviously have to go too (we have more Fortran code than lisp!).

### Unify slicing syntax<a id="sec-4-2-2" name="sec-4-2-2"></a>

Things are currently done using the iter slice macro (and mapslice\*'s), mod-dotimes, and einstein-loop generator. The more 
elegant course to take would be unify these with a nice syntactic glue; sadly as far I know this hasn't been done before. 
This will require quite a bit of prototyping.

### Functionality<a id="sec-4-2-3" name="sec-4-2-3"></a>

-   Make everything in src/old/ compatible with new datastrutures.
-   Add negative stride support, ala Python.
-   Tensor contraction: Hard to do very quickly.
    Might have to copy stuff into a contiguous array; like Femlisp.
-   LAPACK: Add interfaces to remaining functions.
-   DFFTPACK: computing FFTs
-   QUADPACK: Move from f2cl-ed version to the Fortran one.
-   MINPACK: Move from f2cl-ed version to the Fortran one.
-   ODEPACK: Add abstraction for DLSODE, and DLSODAR may others too.

### Gnuplot interface<a id="sec-4-2-4" name="sec-4-2-4"></a>

-   Make gnuplot interface more usable.

### Python-bridge<a id="sec-4-2-5" name="sec-4-2-5"></a>

(C)Python has far too many things, that we cannot even begin to hope to replicate.
Burgled-batteries has a lot of things which could be useful in talking to CPython.

Getting standard-tensor <-> numpy tranlation should be enough. Mostly care about
matplotlib at the moment.

### Support linking to libraries ?<a id="sec-4-2-6" name="sec-4-2-6"></a>

Parse header files with cffi-grovel.

### Documentation, tests<a id="sec-4-2-7" name="sec-4-2-7"></a>

-   Write documentation.
    Fix the formatting for docstrings. Maybe move to TeXinfo (like femlisp).
-   Write tests
    Use cl-rt stuff to write more tests. Probably even add benchmarks.

### Symbolics, AD, more fancy stuff {wishlist}<a id="sec-4-2-8" name="sec-4-2-8"></a>

-   Use things like macrofy to work with Maxima
-   Provide seamless AD, Symbolic differentiation and numerical function calls, ala scmutils.
-   Symbolic stuff tends to fit in easily with the lisp-based BLAS routines.
    Port code from src/classes/symbolic-tensor.lisp