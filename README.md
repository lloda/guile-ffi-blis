
### guile-ffi-blis

This is a set of Guile FFI bindings for the linear algebra subprograms library,
**BLIS**<sup id="a1">[1](#f1)</sup>. It provides operations such as vector dot
product, matrix-vector product, matrix-matrix product, and so on.  These
bindings are for **BLIS**' ‘typed’ API<sup id="a2">[2](#f2)</sup>.

To use the bindings, import `(ffi blis)`. **BLIS** will be loaded from the
default dynamic library path (see ‘Installation notes’ below). There are up to
three bindings for each function, here using `ZGEMM` as an example:

- `bli_zgemm` (raw binding): the raw C function by `pointer->procedure`. Don't
  use this if you aren't familiar with Guile's FFI.

- `blis-zgemm!` (typed binding): takes array arguments of type `'c64` and operates by
  effect, without making copies. All the arguments must be properly sized. For
  convenience, this function returns the output argument.

- `blis-zgemm` (functional binding): takes array arguments of compatible types and
  returns a newly constructed array. The arguments will be converted as
  necessary, which may result in copies.  The returned array will be of `'c64`
  type.

In principle, for the last two bindings, you don't need to care whether your
array is row-major or column-major or what the strides are. The bindings will
extract the required strides from the array arguments<sup id="a3">[3](#f3)</sup>.

If the function doesn't return an array (e.g. `blis-cdot`) then we only provide
two bindings (e.g. `bli_cdot` and `blis-cdot`).

The bindings also provide type generic versions of the functions
(e.g. `blis-dotv` for **BLIS** `blis-sdotv blis-ddotv blis-cdotv
blis-zdotv`). These simply call one of the typed variants according to the type
of the first array argument.

Enter `,help (ffi blis)` at the Guile REPL to list all the bindings
available<sup id="a4">[4](#f4)</sup>.

### Installation notes

`guile-ffi-blis` uses `dynamic-link` to load the dynamic library for **BLIS**,
so the names of the respective library files must be known. The default name
`libblis` can be configured with the environment variable
`GUILE_FFI_BLIS_LIBNAME`.

If your **BLIS** library isn't installed in the default dynamic library search
path, you can configure specific paths for `guile-ffi-blis` with the environment
variable `GUILE_FFI_BLIS_LIBPATH`. There are other variables that control where
`dynamic-link` searches for libraries (`LTDL_LIBRARY_PATH`, `LD_LIBRARY_PATH`)
and you may prefer to set those instead.

A previous version of this library also included **CBLAS** bindings, but now I have moved those to a
separate library (`guile-ffi-cblas`). The following notes on using that previous version on Guix
might still be useful for `guile-ffi-blis`:
[https://notabug.org/ZelphirKaltstahl/guile-ml#using-guile-ffi-cblas](https://notabug.org/ZelphirKaltstahl/guile-ml#using-guile-ffi-cblas).

### Running the tests

The tests use SRFI-64.

```
$GUILE -L mod -s test/test-ffi-blis.scm
```

Depending on your installation (see above) you might need

```
GUILE_FFI_BLIS_LIBNAME=libotherblas \
GUILE_FFI_BLIS_LIBPATH=/custom/path/lib \
$GUILE ... etc.
```

### Coverage

#### BLIS level 1

* `scopyv` `dcopyv` `ccopyv` `zcopyv`
* `sdaxpyv` `ddaxpyv` `cdaxpyv` `zdaxpyv`
* `sdaxpbyv` `ddaxpbyv` `cdaxpbyv` `zdaxpbyv`
* `sdotv` `ddotv` `cdotv` `zdotv`

#### BLIS level 2

* `sgemv` `dgemv` `cgemv` `zgemv`
* `sger` `dger` `cger` `zger`

#### BLIS level 3

* `sgemm` `dgemm` `cgemm` `zgemm`

***

<b id="f1">¹</b> See [https://github.com/flame/blis](https://github.com/flame/blis) and also the related [http://www.netlib.org/blas/](http://www.netlib.org/blas/),  [https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms](https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms). [↩](#a1)

<b id="f2">²</b> See [https://github.com/flame/blis/blob/master/docs/BLISTypedAPI.md](https://github.com/flame/blis/blob/master/docs/BLISTypedAPI.md). [↩](#a2)

<b id="f3">³</b> By default (2020/07) **BLIS** will flag overlapping stride
combinations in any of the array arguments as errors, even when the result is
well defined (some discussion on the topic
[here](https://groups.google.com/forum/#!topic/blis-discuss/ANM7i1ZpuwU)). However,
if you disable **BLIS**' internal error checking with
`(bli-error-checking-level-set BLIS_NO_ERROR_CHECKING)` **BLIS** will produce
the correct result, as far as I've been able to verify. `(ffi blis)` performs
independent shape checks on the typed and functional bindings, and the Guile
array arguments have valid strides by construction, so the lack of error
checking by **BLIS** itself isn't necessarily a problem. The test suite includes
tests with a variety of overlapping stride combinations for `gemm` and
`gemv`. Still, **BLIS** doesn't *officially* support these strides. Note that if
the *destination* argument has overlapping strides, then the result depends on
the order in which the operations are carried out and is pretty much
undefined. `(ffi blis)` will *not* check the destination argument for this
error. [↩](#a3)

<b id="f4">⁴</b> This triggers a bug in the current version. You can use
`bli[TAB]` as a substitute. [↩](#a5)
