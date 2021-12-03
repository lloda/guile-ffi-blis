; -*- mode: scheme; coding: utf-8 -*-
; BLIS FFI module.

; (c) Daniel Llorens - 2014-2015, 2019-2020
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU Lesser General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Access BLIS (https://github.com/flame/blis/blob/master/docs/BLISTypedAPI.md)
;; through Guile's FFI.
;;; Code:

(define-module (ffi blis))
(import (system foreign) (system foreign-library) (ice-9 match)
        (srfi srfi-1) (srfi srfi-11) (srfi srfi-26) (ffi blis arrays))

; TODO As an alternative go through installation.
(define libblis (dynamic-link (let ((lpath (getenv "GUILE_FFI_BLIS_LIBPATH"))
                                     (lname (or (getenv "GUILE_FFI_BLIS_LIBNAME") "libblis")))
                                 (if (and lpath (not (string=? lpath "")))
                                   (string-append lpath file-name-separator-string lname)
                                   lname))))
(dynamic-call "bli_init" libblis)

(define gint_t int64)
(define dim_t gint_t)
(define inc_t gint_t)
(define doff_t gint_t)

(define trans_t int)
(define conj_t int)
(define side_t int)
(define diag_t int)
(define uplo_t int)

(define rank0_t '*)
(define rank1_t '*)
(define rank2_t '*)


; -----------------------------
; wrapper utilities
; -----------------------------

(define (pointer-to-first A)
  (bytevector->pointer (shared-array-root A)
                       (* (shared-array-offset A) (srfi-4-type-size (array-type A)))))

(define (scalar->arg stype a)
  (bytevector->pointer (make-typed-array stype a 1) 0))


; -----------------------------
; BLIS flags
; -----------------------------

; https://github.com/flame/blis/blob/master/docs/BLISTypedAPI.md
; https://github.com/flame/blis/blob/master/frame/include/bli_type_defs.h

; trans_t
(define BLIS_NO_TRANSPOSE 0)
(define BLIS_TRANSPOSE 8)
(define BLIS_CONJ_NO_TRANSPOSE 16)
(define BLIS_CONJ_TRANSPOSE 24)

; conj_t
(define BLIS_NO_CONJUGATE 0)
(define BLIS_CONJUGATE 16)

; diag_t
(define BLIS_NONUNIT_DIAG 0)
(define BLIS_UNIT_DIAG 256)

; uplo_t
(define BLIS_ZEROS 0)
(define BLIS_LOWER 192)
(define BLIS_UPPER 96)
(define BLIS_DENSE 224)

; side_t
(define BLIS_LEFT 0)
(define BLIS_RIGHT 1)

(define (fliptr t)
  (cond
   ((= t BLIS_NO_TRANSPOSE) BLIS_TRANSPOSE)
   ((= t BLIS_TRANSPOSE) BLIS_NO_TRANSPOSE)
   ((= t BLIS_CONJ_NO_TRANSPOSE) BLIS_CONJ_TRANSPOSE)
   ((= t BLIS_CONJ_TRANSPOSE) BLIS_CONJ_NO_TRANSPOSE)
   (else (throw 'bad-transpose-1 t))))

(define (tr? t)
  (cond
   ((= t BLIS_NO_TRANSPOSE) #f)
   ((= t BLIS_TRANSPOSE) #t)
   ((= t BLIS_CONJ_NO_TRANSPOSE) #f)
   ((= t BLIS_CONJ_TRANSPOSE) #t)
   (else (throw 'bad-transpose-2 t))))

(export BLIS_NO_TRANSPOSE BLIS_TRANSPOSE BLIS_CONJ_NO_TRANSPOSE BLIS_CONJ_TRANSPOSE tr? fliptr
        BLIS_NO_CONJUGATE BLIS_CONJUGATE
        BLIS_NONUNIT_DIAG BLIS_UNIT_DIAG
        BLIS_ZEROS BLIS_LOWER BLIS_UPPER BLIS_DENSE
        BLIS_LEFT BLIS_RIGHT)


; -----------------------------
; BLIS configuration
; -----------------------------

(define BLIS_NO_ERROR_CHECKING 0)
(define BLIS_FULL_ERROR_CHECKING 1)

(define bli_error_checking_level_set
  (pointer->procedure void (dynamic-func "bli_error_checking_level_set" libblis) (list gint_t)))
(define (blis-error-checking-level-set! level)
  (unless (<= 0 level 1) (throw 'invalid-error-checking-level level))
  (bli_error_checking_level_set level))

(export BLIS_NO_ERROR_CHECKING BLIS_FULL_ERROR_CHECKING
        blis-error-checking-level-set!)


; -----------------------------
; level-1v: addv amaxv *axpyv *axpbyv *copyv *dotv dotxv invertv scal2v scalv setv subv swapv xpbyv
; -----------------------------

#|
x := conjalpha(alpha)

void bli_?setv
     (
       conj_t  conjalpha,
       dim_t   n,
       ctype*  alpha,
       ctype*  x, inc_t incx
     );
|#

(define-syntax define-setv
  (lambda (x)
    (syntax-case x ()
      ((_ type_ blis-name name!)
       (with-syntax ((type #'(quote type_)))
         #`(begin
             (define blis-name (pointer->procedure
                                void (dynamic-func #,(symbol->string (syntax->datum #'blis-name)) libblis)
                                (list conj_t dim_t rank0_t rank1_t inc_t)))
             (define (name! conjalpha alpha X)
               #,(let ((t (syntax->datum #'type_)))
                   (format #f "(~a conjalpha alpha x) => y\n\n~a"
                           (symbol->string (syntax->datum #'name!))
                           "x := conjalpha(alpha)"))
               (check-array X 1 type)
               (blis-name conjalpha (array-length X) (scalar->arg type alpha)
                          (pointer-to-first X) (stride X 0))
               X)))))))

(define-sdcz setv bli_?setv blis-?setv!)
(define-auto (blis-setv! conjalpha alpha X) X blis-?setv!)

#|
A := conjalpha(alpha)

void bli_?setm
     (
       conj_t  conjalpha,
       doff_t  diagoffa,
       diag_t  diaga,
       uplo_t  uploa,
       dim_t   m,
       dim_t   n,
       ctype*  alpha,
       ctype*  a, inc_t rsa, inc_t csa
     );
|#

(define-syntax define-setm
  (lambda (x)
    (syntax-case x ()
      ((_ type_ blis-name name!)
       (with-syntax ((type #'(quote type_)))
         #`(begin
             (define blis-name (pointer->procedure
                                void (dynamic-func #,(symbol->string (syntax->datum #'blis-name)) libblis)
                                (list conj_t doff_t diag_t uplo_t dim_t dim_t rank0_t rank2_t inc_t inc_t)))
             (define (name! conjalpha diagoffa diaga uploa alpha A)
               #,(let ((t (syntax->datum #'type_)))
                   (format #f "(~a conjalpha diagoffa diaga uploa alpha A) => A\n\n~a"
                           (symbol->string (syntax->datum #'name!))
                           "A := conjalpha(alpha)"))
               (check-array A 2 type)
               (blis-name conjalpha diagoffa diaga uploa (dim A 0) (dim A 1) (scalar->arg type alpha)
                          (pointer-to-first A) (stride A 0) (stride A 1))
               A)))))))

(define-sdcz setm bli_?setm blis-?setm!)
(define-auto (blis-setm! conjalpha diagoffa diaga uploa alpha A) A blis-?setm!)

#|
y := conjx(x)

void bli_?copyv
     (
       conj_t  conjx,
       dim_t   n,
       ctype*  alpha,
       ctype*  x, inc_t incx,
       ctype*  beta,
       ctype*  y, inc_t incy
     )
|#

(define-syntax define-copyv
  (lambda (x)
    (syntax-case x ()
      ((_ type_ blis-name name!)
       (with-syntax ((type #'(quote type_)))
         #`(begin
             (define blis-name (pointer->procedure
                                void (dynamic-func #,(symbol->string (syntax->datum #'blis-name)) libblis)
                                (list conj_t dim_t rank1_t inc_t rank1_t inc_t)))
             (define (name! conjX X Y)
               #,(let ((t (syntax->datum #'type_)))
                   (format #f "(~a conjx [conj_t] x [#~a(…)] y [#~a(…)]) => y\n\n~a"
                           (symbol->string (syntax->datum #'name!)) t t
                           "y := conjx(x)"))
               (check-2-arrays X Y 1 type)
               (blis-name conjX (array-length X)
                          (pointer-to-first X) (stride X 0)
                          (pointer-to-first Y) (stride Y 0))
               Y)))))))

(define-sdcz copyv bli_?copyv blis-?copyv!)
(define-auto (blis-copyv! conjX X Y) X blis-?copyv!)

#|
y := conjx(x)

void bli_?copym
     (
       conj_t  conjx,
       dim_t   n,
       ctype*  alpha,
       ctype*  x, inc_t incx,
       ctype*  beta,
       ctype*  y, inc_t incy
     )
|#

(define-syntax define-copym
  (lambda (x)
    (syntax-case x ()
      ((_ type_ blis-name name!)
       (with-syntax ((type #'(quote type_)))
         #`(begin
             (define blis-name (pointer->procedure
                                void (dynamic-func #,(symbol->string (syntax->datum #'blis-name)) libblis)
                                (list doff_t diag_t uplo_t trans_t dim_t dim_t
                                      rank2_t inc_t inc_t rank2_t inc_t inc_t)))
             (define (name! diagoffa diaga uploa transa A B)
               #,(let ((t (syntax->datum #'type_)))
                   (format #f "(~a diagoffa diaga uploa transa A B) => B\n\n~a"
                           (symbol->string (syntax->datum #'name!))
                           "B := transa(A)"))
               (check-array A 2 type)
               (check-array B 2 type)
               (let ((M (dim B 0))
                     (N (dim B 1)))
                 (unless (= M (dim A (if (tr? transa) 1 0))) (throw 'mismatched-B-rows))
                 (unless (= N (dim A (if (tr? transa) 0 1))) (throw 'mismatched-B-columns))
                 (blis-name diagoffa diaga uploa transa M N
                            (pointer-to-first A) (stride A 0) (stride A 1)
                            (pointer-to-first B) (stride B 0) (stride B 1))
                 B))))))))

(define-sdcz copym bli_?copym blis-?copym!)
(define-auto (blis-copym! diagoffa diaga uploa transa A B) A blis-?copym!)

#|
void bli_?axpyv
     (
       conj_t  conjx,
       dim_t   n,
       ctype*  alpha,
       ctype*  x, inc_t incx,
       ctype*  y, inc_t incy
     )
|#

(define-syntax define-axpyv
  (lambda (x)
    (syntax-case x ()
      ((_ type_ blis-name name!)
       (with-syntax ((type #'(quote type_)))
         #`(begin
             (define blis-name (pointer->procedure
                                void (dynamic-func (symbol->string (syntax->datum #'blis-name)) libblis)
                                (list conj_t dim_t rank0_t rank1_t inc_t rank1_t inc_t)))
             (define (name! conjX alpha X Y)
               #,(let ((t (syntax->datum #'type_)))
                   (format #f "(~a conjx [conj_t] alpha [~a] x [#~a(…)] y [#~a(…)]) => y\n\n~a"
                           (symbol->string (syntax->datum #'name!)) t t t
                           "y := y + alpha * conjx(x)"))
               (check-2-arrays X Y 1 type)
               (blis-name conjX (array-length X)
                          (scalar->arg type alpha)
                          (pointer-to-first X) (stride X 0)
                          (pointer-to-first Y) (stride Y 0))
               Y)))))))

(define-sdcz axpyv bli_?axpyv blis-?axpyv!)
(define-auto (blis-axpyv! conjX alpha X Y) X blis-?axpyv!)

#|
B := B + alpha * transa(A)

void bli_?axpym
     (
       doff_t  diagoffa,
       diag_t  diaga,
       uplo_t  uploa,
       trans_t transa,
       dim_t   m,
       dim_t   n,
       ctype*  alpha,
       ctype*  a, inc_t rsa, inc_t csa,
       ctype*  b, inc_t rsb, inc_t csb
     )
|#

(define-syntax define-axpym
  (lambda (x)
    (syntax-case x ()
      ((_ type_ blis-name name!)
       (with-syntax ((type #'(quote type_)))
         #`(begin
             (define blis-name (pointer->procedure
                                void (dynamic-func (symbol->string (syntax->datum #'blis-name)) libblis)
                                (list doff_t diag_t uplo_t trans_t dim_t dim_t rank0_t
                                      rank2_t inc_t inc_t
                                      rank2_t inc_t inc_t)))
             (define (name! diagoffa diaga uploa transa alpha A B)
               #,(let ((t (syntax->datum #'type_)))
                   (format #f "(~a diagoffa diaga uploa transa alpha A B) => B\n\n~a"
                           (symbol->string (syntax->datum #'name!))
                           "B := B + alpha * transa(A)"))
               (check-array A 2 type)
               (check-array B 2 type)
               (let ((M (dim B 0))
                     (N (dim B 1)))
                 (unless (= M (dim A (if (tr? transa) 1 0))) (throw 'mismatched-B-rows))
                 (unless (= N (dim A (if (tr? transa) 0 1))) (throw 'mismatched-B-columns))
                 (blis-name diagoffa diaga uploa transa M N (scalar->arg type alpha)
                            (pointer-to-first A) (stride A 0) (stride A 1)
                            (pointer-to-first B) (stride B 0) (stride B 1))
                 B))))))))

(define-sdcz axpym bli_?axpym blis-?axpym!)
(define-auto (blis-axpym! diagoffa diaga uploa transa alpha A B) A blis-?axpym!)

#|
y := beta * y + alpha * conjx(x)

void bli_?axpbyv
     (
       conj_t  conjx,
       dim_t   n,
       ctype*  alpha,
       ctype*  x, inc_t incx,
       ctype*  beta,
       ctype*  y, inc_t incy
     )
|#

(define-syntax define-axpbyv
  (lambda (x)
    (syntax-case x ()
      ((_ type_ blis-name name!)
       (with-syntax ((type #'(quote type_)))
         #`(begin
             (define blis-name (pointer->procedure
                                void (dynamic-func #,(symbol->string (syntax->datum #'blis-name)) libblis)
                                (list conj_t dim_t rank0_t rank1_t inc_t rank0_t rank1_t inc_t)))
             (define (name! conjX alpha X beta Y)
               #,(let ((t (syntax->datum #'type_)))
                   (format #f "(~a conjx [conj_t] alpha [~a] x [#~a(…)] beta [~a] y [#~a(…)]) => y\n\n~a"
                           (symbol->string (syntax->datum #'name!)) t t t t
                           "y := beta * y + alpha * conjx(x)"))
               (check-2-arrays X Y 1 type)
               (blis-name conjX (array-length X)
                          (scalar->arg type alpha)
                          (pointer-to-first X) (stride X 0)
                          (scalar->arg type beta)
                          (pointer-to-first Y) (stride Y 0))
               Y)))))))

(define-sdcz axpbyv bli_?axpbyv blis-?axpbyv!)
(define-auto (blis-axpbyv! conjX alpha X beta Y) X blis-?axpbyv!)


#|
void bli_?dotv
     (
       conj_t  conjx,
       conj_t  conjy,
       dim_t   n,
       ctype*  x, inc_t incx,
       ctype*  y, inc_t incy,
       ctype*  rho
       );
|#

(define-syntax define-dotv
  (lambda (x)
    (syntax-case x ()
      ((_ type_ blis-name name)
       (with-syntax ((type #'(quote type_)))
         #`(begin
             (define blis-name (pointer->procedure
                                void (dynamic-func #,(symbol->string (syntax->datum #'blis-name)) libblis)
                                (list conj_t conj_t dim_t rank1_t inc_t rank1_t inc_t rank0_t)))
             (define (name conjX conjY X Y)
               #,(let ((t (syntax->datum #'type_)))
                   (format #f "(~a conjx [conj_t] conjy [conj_t] x [#~a(…)] y [#~a(…)]) => rho\n\n~a"
                           (symbol->string (syntax->datum #'name)) t t
                           "rho := conjX(X)^T * conjY(Y)"))
               (check-2-arrays X Y 1 type)
               (let ((rho (make-typed-array type 0)))
                 (blis-name conjX conjY (array-length X)
                            (pointer-to-first X) (stride X 0)
                            (pointer-to-first Y) (stride Y 0)
                            (pointer-to-first rho))
                 (array-ref rho)))))))))

(define-sdcz dotv bli_?dotv blis-?dotv)
(define-auto (blis-dotv conjX conjY X Y) X blis-?dotv)


; -----------------------------
; level-2: *gemv *ger hemv her her2 symv syr syr2 trmv trsv
; -----------------------------

#|
void bli_?gemv( trans_t transa,
                conj_t  conjx,
                dim_t   m,
                dim_t   n,
                ctype*  alpha,
                ctype*  a, inc_t rsa, inc_t csa,
                ctype*  x, inc_t incx,
                ctype*  beta,
                ctype*  y, inc_t incy );
|#

(define-syntax define-gemv
  (lambda (x)
    (syntax-case x ()
      ((_ type blis-name name! name)
       (with-syntax ((type #'(quote type)))
         #`(begin
             (define blis-name (pointer->procedure
                                void (dynamic-func #,(symbol->string (syntax->datum #'blis-name)) libblis)
                                (list trans_t conj_t dim_t dim_t
                                      rank0_t rank2_t inc_t inc_t
                                      rank1_t inc_t
                                      rank0_t rank1_t inc_t)))
             (define (name! transA conjX alpha A X beta Y)
               #,(format #f "\
(~a transA conjX alpha A X beta Y)

  Y := beta * Y + alpha * transA(A) * conjX(X)

See also: ~a"
                         (syntax->datum #'name!) (syntax->datum #'name))
               (check-array A 2 type)
               (check-array X 1 type)
               (check-array Y 1 type)
               (let ((M (array-length Y))
                     (N (array-length X)))
                 (unless (= M (dim A (if (tr? transA) 1 0))) (throw 'mismatched-YA))
                 (unless (= N (dim A (if (tr? transA) 0 1))) (throw 'mismatched-XA))
                 (blis-name transA conjX M N
                            (scalar->arg type alpha)
                            (pointer-to-first A) (stride A 0) (stride A 1)
                            (pointer-to-first X) (stride X 0)
                            (scalar->arg type beta)
                            (pointer-to-first Y) (stride Y 0))
                 Y))
             (define (name transA conjX alpha A X)
               #,(format #f "\
(~a transA conjX alpha A X beta Y)

Return

  alpha * transA(A) * conjX(X)

as a new array.

See also: ~a"
                         (syntax->datum #'name) (syntax->datum #'name!))
               (let ((Y (make-typed-array type *unspecified*
                                          (dim A (if (tr? transA) 1 0)))))
                 (name! transA conjX alpha A X 0 Y)))))))))

(define-sdcz gemv bli_?gemv blis-?gemv! blis-?gemv)
(define-auto (blis-gemv! transA conjX alpha A X beta Y) A blis-?gemv!)

#|
void bli_?ger( conj_t  conjx,
               conj_t  conjy,
               dim_t   m,
               dim_t   n,
               ctype*  alpha,
               ctype*  x, inc_t incx,
               ctype*  y, inc_t incy,
               ctype*  a, inc_t rsa, inc_t csa );
|#

(define-syntax define-ger
  (lambda (x)
    (syntax-case x ()
      ((_ type blis-name name! name)
       (with-syntax ((type #'(quote type)))
         #`(begin
             (define blis-name (pointer->procedure
                                void (dynamic-func #,(symbol->string (syntax->datum #'blis-name)) libblis)
                                (list conj_t conj_t dim_t dim_t
                                      rank0_t rank1_t inc_t rank1_t inc_t
                                      rank2_t inc_t inc_t)))
             (define (name! conjX conjY alpha X Y A)
               #,(format #f "\
(~a conjX conjY alpha X Y A)

  A := A + alpha * conjX(X) * conjY(Y)^T

See also: ~a"
                         (syntax->datum #'name!) (syntax->datum #'name))
               (check-array A 2 type)
               (check-array X 1 type)
               (check-array Y 1 type)
               (let ((M (array-length X))
                     (N (array-length Y)))
                 (unless (= M (dim A 0)) (throw 'mismatched-XA))
                 (unless (= N (dim A 1)) (throw 'mismatched-YA))
                 (blis-name conjX conjY (array-length X) (array-length Y)
                            (scalar->arg type alpha)
                            (pointer-to-first X) (stride X 0)
                            (pointer-to-first Y) (stride Y 0)
                            (pointer-to-first A) (stride A 0) (stride A 1))
                 A))
             (define (name conjX conjY alpha X Y)
               #,(format #f "\
(~a conjX conjY alpha X Y)

Return

  alpha * transA(A) * conjX(X)

as a new array.

See also: ~a"
                         (syntax->datum #'name) (syntax->datum #'name!))
               (let ((A (make-typed-array type 0 (array-length X) (array-length Y))))
                 (name! conjX conjY alpha X Y A)))))))))

(define-sdcz ger bli_?ger blis-?ger! blis-?ger)
(define-auto (blis-ger! conjX conjY alpha X Y A) X blis-?ger!)


; -----------------------------
; level-3: *gemm hemm herk her2k symm syrk syr2k trmm trmm3 trsm
; -----------------------------

#|
void bli_?gemm( trans_t transa,
                trans_t transb,
                dim_t   m,
                dim_t   n,
                dim_t   k,
                ctype*  alpha,
                ctype*  a, inc_t rsa, inc_t csa,
                ctype*  b, inc_t rsb, inc_t csb,
                ctype*  beta,
                ctype*  c, inc_t rsc, inc_t csc )
|#

(define-syntax define-gemm
  (lambda (x)
    (syntax-case x ()
      ((_ type blis-name name! name)
       (with-syntax ((type #'(quote type)))
         #`(begin
             (define blis-name (pointer->procedure
                                void (dynamic-func #,(symbol->string (syntax->datum #'blis-name)) libblis)
                                (list trans_t trans_t dim_t dim_t dim_t
                                      rank0_t rank2_t inc_t inc_t
                                      rank2_t inc_t inc_t
                                      rank0_t rank2_t inc_t inc_t)))
             (define (name! transA transB alpha A B beta C)
               #,(format #f "\
(~a transA transB alpha A B beta C)

  C := beta * C + alpha * transA(A) * transB(B)

See also: ~a
"
                         (syntax->datum #'name!) (syntax->datum #'name))
               (check-array A 2 type)
               (check-array B 2 type)
               (check-array C 2 type)
               (let ((M (dim C 0))
                     (N (dim C 1))
                     (K (dim A (if (tr? transA) 0 1))))
                 (unless (= M (dim A (if (tr? transA) 1 0))) (throw 'mismatched-CA))
                 (unless (= N (dim B (if (tr? transB) 0 1))) (throw 'mismatched-CB))
                 (unless (= K (dim B (if (tr? transB) 1 0))) (throw 'mismatched-AB))
                 (blis-name transA transB M N K
                            (scalar->arg type alpha)
                            (pointer-to-first A) (stride A 0) (stride A 1)
                            (pointer-to-first B) (stride B 0) (stride B 1)
                            (scalar->arg type beta)
                            (pointer-to-first C) (stride C 0) (stride C 1))
                 C))
             (define (name transA transB alpha A B)
               #,(format #f "\
(~a transA transB alpha A B)

Return

  alpha * transA(A) * transB(B)

as a new array.

See also: ~a
"
                         (syntax->datum #'name) (syntax->datum #'name!))
               (let ((C (make-typed-array type *unspecified*
                                          (dim A (if (tr? transA) 1 0))
                                          (dim B (if (tr? transB) 0 1)))))
                 (name! transA transB alpha A B 0. C)))))))))

(define-sdcz gemm bli_?gemm blis-?gemm! blis-?gemm)
(define-auto (blis-gemm! transA transB alpha A B beta C) A blis-?gemm!)
