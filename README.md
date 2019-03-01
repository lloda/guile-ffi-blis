
### guile-ffi-cblas

This is a set of Guile FFI bindings for two libraries of linear algebra
subprograms, **CBLAS** and **BLIS**. They provide operations such as vector dot
product, matrix-vector product, matrix-matrix product, and so on.

The bindings for either library are entirely independent. You do not need to
have **BLIS** installed to use the **CBLAS** bindings or viceversa. I am
packaging them together because the bindings happen to share a lot of the code.

**CBLAS** (or **BLAS**) is an established standard. It's by far the more popular
library, and it probably has the fastest implementation on your
system. However **BLIS** is much more regular and nicer to program against.

Both sets of bindings work in the same way. Upon importing `(ffi cblas)` (or
`(ffi blis)`), the bindings will attempt to load **CBLAS** (or **BLIS**)
dynamically from the default dynamic library path. If your **CBLAS** (or
**BLIS**) is somewhere else, you can specify a path with the environment
variable `GUILE_FFI_CBLAS_LIBCBLAS_PATH` (or
`GUILE_FFI_CBLAS_LIBBLIS_PATH`). You can configure the name of the library
itself with `GUILE_FFI_CBLAS_LIBCBLAS_NAME` (or
`GUILE_FFI_CBLAS_LIBBLIS_NAME`). The default name is `libcblas` (or
`libblis`). For example, on my system, the **CBLAS** library is actually called
`libtatlas`.

There are up to three bindings for each function, here using `ZGEMM` as an
example:

- `cblas_zgemm` (raw binding): the raw C function by `pointer->procedure`. Don't
  use this if you aren't familiar with Guile's FFI.

- `zgemm!` (typed binding): takes array arguments of type `'c64` and operates by
  effect, without making copies. All the arguments must be properly sized. The
  return value is unspecified.

- `zgemm` (functional binding): takes array arguments of compatible types and
  returns a newly constructed array. The arguments will be converted as
  necessary, which may result in copies.  The returned array will be of `'c64`
  type.

In principle, for the last two bindings, you don't need to care whether your
array is row-major or column-major or what the strides are. The bindings will
extract the required stride arguments from the array descriptors. However, since
**CBLAS** doesn't support arbitrary strides (e.g. it only supports a column
stride for matrix arguments, assuming column-major order), some array arguments
will cause the typed binding to fail, or result in extra copies with the
functional binding. **BLIS** doesn't have this problem.

Note that these bindings are a work in progress and that there are bugs. For
example, negative strides require specific handling for **CBLAS** and are not
supported yet. Once again, **BLIS** doesn't mess up negative strides, so it avoids
this problem.

---

### Running the tests

```
$GUILE -L mod -s test/test-ffi-cblas.scm
$GUILE -L mod -s test/test-ffi-blis.scm
```

You might need

```
GUILE_FFI_CBLAS_LIBCBLAS_NAME=libblas GUILE_FFI_CBLAS_LIBCBLAS_PATH=/custom/path/lib $GUILE ... etc.
```

---

### Coverage

#### CBLAS level 1

* srotg drotg crotg zrotg
* sscal dscal cscal zscal csscal zdscal
* sswap dswap cswap zswap
* scopy dcopy ccopy zcopy
* saxpy daxpy caxpy zaxpy
* sdot ddot cdotu zdotu cdotc zdotc
* snrm2 dnrm2 scnrm2 dznrm2
* sasum dasum scasum dzasum
* isamax idamax icamax izamax

#### CBLAS level 2

* sgemv dgemv cgemv zgemv
* sger dger cgeru zgeru cgerc zgerc

#### CBLAS level 3

* sgemm dgemm cgemm zgemm

#### CBLIS level 2

* sgemv dgemv cgemv zgemv
* sger dger cger zger

#### CBLIS level 3

* sgemm dgemm cgemm zgemm
