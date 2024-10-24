; -*- mode: scheme; coding: utf-8 -*-
; Tests for (ffi blis).

; (c) Daniel Llorens - 2014-2015, 2019-2020
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU Lesser General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

(import (ffi blis) (srfi srfi-64) (srfi srfi-1) (ice-9 match) (srfi srfi-26) (ice-9 arrays))
(include "common.scm")

(test-begin "ffi-blis")

(define (apply-transpose-flag A flag)
  (cond ((or (eq? flag BLIS-NO-TRANSPOSE) (eq? flag BLIS-NO-CONJUGATE)) A)
        ((eq? flag BLIS-TRANSPOSE) (transpose-array A 1 0))
        ((or (eq? flag BLIS-CONJ-NO-TRANSPOSE) (eq? flag BLIS-CONJUGATE))
         (let ((B (array-copy A))) (array-map! B conj A) B))
        ((eq? flag BLIS-CONJ-TRANSPOSE)
         (let ((B (array-copy A))) (array-map! B conj A) (transpose-array B 1 0)))
        (else (throw 'bad-transpose-flag flag))))

; to be disabled/relaxed for specific tests, see below
(blis-error-checking-level-set! BLIS_FULL_ERROR_CHECKING)


; ---------------------------------
; Test types
; ---------------------------------

(define-syntax for-each-lambda
  (lambda (x)
    (syntax-case x ()
      ((_ ((a b) ...) e0 e ...)
       #'(for-each (lambda (a ...) e0 e ...) b ...)))))

(define* (test-approximate-array tag expected val err)
  (let ((tag (if (symbol? tag) (symbol->string tag) tag)))
    (test-begin tag)
    (array-for-each (lambda (expected val)
                      (if (and (real? expected) (real? val))
                        (test-approximate expected val err)
                        (test-approximate 0. (magnitude (- expected val)) err)))
                    expected val)
    (test-end tag)))

(define (scalar-cases stype)
  (match stype
    ((or 'f32 'f64) '(-1 0 2))
    ((or 'c32 'c64) '(1-1i 1+1i 0 2))))


; ---------------------------------
; ?amaxv
; ---------------------------------

(test-equal 6 (blis-amaxv #c64(1 2 3 4 2 -1 -8 3+3i)))
(test-equal 7 (blis-amaxv #c64(1 2 3 4 2 -1 -8 5+5i)))


; ---------------------------------
; ?setv
; ---------------------------------

(let* ((X (array-copy #f64(1 2 3 4)))
       (Y (blis-dsetv! BLIS-NO-CONJUGATE 3. X)))
  (test-eq X Y)
  (test-equal X (make-typed-array 'f64 3 4)))

(let* ((X (array-copy #c64(1 2 3 4)))
       (Y (blis-zsetv! BLIS-CONJUGATE 3+9i X)))
  (test-eq X Y)
  (test-equal X (make-typed-array 'c64 3-9i 4)))

(let* ((X (array-copy #c64(1 2 3 4)))
       (Y (blis-setv! BLIS-NO-CONJUGATE 3+9i X)))
  (test-eq X Y)
  (test-equal X (make-typed-array 'c64 3+9i 4)))


; ---------------------------------
; ?setm
; ---------------------------------

(let* ((A (array-copy #2f64((1 2 3) (4 5 6))))
       (B (blis-dsetm! BLIS-NO-CONJUGATE 0 BLIS-NONUNIT-DIAG BLIS-DENSE 3. A)))
  (test-eq A B)
  (test-equal A (make-typed-array 'f64 3. 2 3)))

(let* ((A (array-copy #2c64((1 2 3) (4 5 6))))
       (B (blis-setm! BLIS-CONJUGATE 0 BLIS-NONUNIT-DIAG BLIS-DENSE 3+9i A)))
  (test-eq A B)
  (test-equal A (make-typed-array 'c64 3-9i 2 3)))


; ---------------------------------
; ?copyv ?axbyv ?axpbyv
; ---------------------------------

(define (test-lin type f-name f conj-A alpha make-A beta make-B)

  (define (ref conjX alpha X beta Y)
    (array-map! Y
      (lambda (x y)
        (+ (* beta y) (* alpha (if (eqv? conjX BLIS-CONJUGATE) (conj x) x))))
      X Y)
    Y)

  (let* ((tag (format #f "~a:~a" (procedure-name make-A) (procedure-name make-B)))
         (case-name (format #f "~a, ~a" (procedure-name f) tag))
         (A (fill-A1! (make-A type)))
         (B (fill-B1! (make-B type)))
         (Aref (array-copy A))
         (Bref (array-copy B)))
    (test-begin case-name)
    (for-each-lambda ((alpha alpha))
      (for-each-lambda ((beta beta))
        (let ((val-ref (ref conj-A alpha A beta Bref))
              (val-f (f conj-A alpha A beta B)))
          (test-approximate-array 'source Aref A 0)
          (test-approximate-array 'content Bref B 0)
          (test-approximate-array 'result val-ref val-f 0))))
    (test-end case-name)))

(for-each-lambda ((type '(f32 f64 c32 c64))
                  (copyv (list blis-scopyv! blis-dcopyv! blis-ccopyv! blis-zcopyv!))
                  (axpyv (list blis-saxpyv! blis-daxpyv! blis-caxpyv! blis-zaxpyv!))
                  (axpbyv (list blis-saxpbyv! blis-daxpbyv! blis-caxpbyv! blis-zaxpbyv!)))
  (let ((scalar-cases (scalar-cases type)))
    (for-each (match-lambda
                ((conj-A make-A make-B)
                 (test-lin type 'copy
                           (lambda (conj-A alpha make-A beta make-B)
                             (copyv conj-A make-A make-B))
                           conj-A '(1) make-A '(0) make-B)
                 (test-lin type 'axpyv
                           (lambda (conj-A alpha make-A beta make-B)
                             (axpyv conj-A alpha make-A make-B))
                           conj-A scalar-cases make-A '(1) make-B)
                 (test-lin type 'axpbyv
                           axpbyv
                           conj-A scalar-cases make-A scalar-cases make-B)))
      (list-product
       (list BLIS-CONJUGATE BLIS-NO-CONJUGATE)
       (list make-v-compact make-v-offset make-v-strided)
       (list make-v-compact make-v-offset make-v-strided)))))


; ---------------------------------
; ?swapv
; ---------------------------------

(let* ((x (array-copy #f64(1 2 3)))
       (z (array-copy #f64(7 8 9 10 11 12)))
       (y (make-shared-array z (lambda (i) (list (+ 1 (* i 2)))) 3)))
  (blis-swapv! x y)
  (test-assert (array-equal? x #f64(8 10 12)))
  (test-assert (array-equal? z #f64(7 1 9 2 11 3))))


(let* ((A (array-copy #2c64((1 2 3) (4 5 6))))
       (B (blis-setm! BLIS-CONJUGATE 0 BLIS-NONUNIT-DIAG BLIS-DENSE 3+9i A)))
  (test-eq A B)
  (test-equal A (make-typed-array 'c64 3-9i 2 3)))


; ---------------------------------
; ?axpbym ?copym FIXME coverage of flags
; ---------------------------------

(define A (array-copy #2f64((1 2) (3 4))))
(let ((B (array-copy #2f64((9 8) (7 6)))))
  (blis-daxpym! 0 BLIS-NONUNIT-DIAG BLIS-DENSE BLIS-NO-TRANSPOSE 3 A B)
  (test-equal B #2f64((12. 14.) (16. 18.)))
  (blis-daxpym! 0 BLIS-NONUNIT-DIAG BLIS-DENSE BLIS-TRANSPOSE 3 A B)
  (test-equal B #2f64((15. 23.) (22. 30.)))
  (let ((C (array-copy A)))
    (blis-dcopym! 0 BLIS-NONUNIT-DIAG BLIS-DENSE BLIS-TRANSPOSE B C)
    (test-equal B #2f64((15. 23.) (22. 30.)))
    (test-equal C #2f64((15. 22.) (23. 30.)))))


; ---------------------------------
; ?dotv
; ---------------------------------

(define (test-dotv type f conj-A conj-B make-A make-B)

  (define (ref conj-A conj-B A B)
    (let ((rho 0))
      (array-for-each
       (lambda (a b)
         (set! rho (+ rho (* (if (eq? conj-A BLIS-CONJUGATE) (conj a) a)
                             (if (eq? conj-B BLIS-CONJUGATE) (conj b) b)))))
       A B)
      rho))

  (let* ((tag (format #f "~a:~a" (procedure-name make-A) (procedure-name make-B)))
         (case-name (format #f "~a, ~a" (procedure-name f) tag))
         (A (fill-A1! (make-A type)))
         (B (fill-B1! (make-B type))))
    (test-begin case-name)
    (test-equal (ref conj-A conj-B A B) (f conj-A conj-B A B))
    (test-end case-name)))

(for-each-lambda ((type '(f32 f64 c32 c64))
           (dotv (list blis-sdotv blis-ddotv blis-cdotv blis-zdotv)))
  (for-each (match-lambda
              ((conj-A conj-B make-A make-B)
               (test-dotv type blis-dotv conj-A conj-B make-A make-B)))
    (list-product
     (list BLIS-CONJUGATE BLIS-NO-CONJUGATE)
     (list BLIS-CONJUGATE BLIS-NO-CONJUGATE)
     (list make-v-compact make-v-offset make-v-strided)
     (list make-v-compact make-v-offset make-v-strided))))


; ---------------------------------
; ?norm1v normfv normiv
; ---------------------------------

(test-approximate (blis-dnorm1v #f64(1 2 3 4)) 10. 0)
(test-approximate (blis-dnormfv #f64(1 2 3 4)) (sqrt (+ (* 1 1) (* 2 2) (* 3 3) (* 4 4))) 0)
(test-approximate (blis-dnormiv #f64(1 2 3 4)) 4. 0)
(test-approximate (blis-norm1v #c32(0+1i 2 3 0-4i)) 10. 0)
(test-approximate (blis-normfv #c64(0+1i 2 3 0-4i)) (sqrt (+ (* 1 1) (* 2 2) (* 3 3) (* 4 4))) 0)
(test-approximate (blis-normiv #c32(0+1i 2 3 0-4i)) 4. 0)


; ---------------------------------
; sgemm dgemm cgemm zgemm
; ---------------------------------

(define (test-gemm tag gemm! transA transB alpha A B beta C)

  ;; alpha * sum_k(A_{ik}*B_{kj}) + beta * C_{ij} -> C_{ij}
  (define (ref-gemm! transA transB alpha A B beta C)
    (let* ((A (apply-transpose-flag A transA))
           (B (apply-transpose-flag B transB))
           (M (first (array-dimensions C)))
           (N (second (array-dimensions C)))
           (K (first (array-dimensions B))))
      (do ((i 0 (+ i 1))) ((= i M))
        (do ((j 0 (+ j 1))) ((= j N))
          (array-set! C (* beta (array-ref C i j)) i j)
          (do ((k 0 (+ k 1))) ((= k K))
            (array-set! C (+ (array-ref C i j) (* alpha (array-ref A i k) (array-ref B k j))) i j))))))

  (let ((C1 (array-copy C))
        (C2 (array-copy C))
        (AA (array-copy A))
        (BB (array-copy B)))
    (gemm! transA transB alpha A B beta C1)
    (ref-gemm! transA transB alpha A B beta C2)
    ;; (test-approximate-array tag C1 C2 1e-15) ; TODO as a single test.
    (test-begin tag)
    (test-equal C1 C2)
    (test-equal AA A)
    (test-equal BB B)
    (test-end tag)))

(for-each
 (match-lambda
     ((type gemm!)
; some extra tests with non-square matrices.
      (let ((A (fill-A2! (make-typed-array type *unspecified* 4 3)))
            (B (fill-A2! (make-typed-array type *unspecified* 3 5)))
            (C (fill-A2! (make-typed-array type *unspecified* 4 5))))
        (test-gemm "gemm-1" blis-gemm! BLIS-NO-TRANSPOSE BLIS-NO-TRANSPOSE 1. A B 1. C)
        (test-gemm "gemm-2" blis-gemm! BLIS-TRANSPOSE BLIS-NO-TRANSPOSE 1. A C 1. B)
        (test-gemm "gemm-3" blis-gemm! BLIS-NO-TRANSPOSE BLIS-TRANSPOSE 1. C B 1. A))
      (let ((A (fill-A2! (transpose-array (make-typed-array 'f64 *unspecified* 4 3) 1 0)))
            (B (fill-A2! (transpose-array (make-typed-array 'f64 *unspecified* 3 5) 1 0)))
            (C (fill-A2! (transpose-array (make-typed-array 'f64 *unspecified* 4 5) 1 0))))
        (test-gemm "gemm-4" blis-dgemm! BLIS-TRANSPOSE BLIS-TRANSPOSE 1. A B 1. (transpose-array C 1 0))
        (test-gemm "gemm-5" blis-dgemm! BLIS-NO-TRANSPOSE BLIS-TRANSPOSE 1. A C 1. (transpose-array B 1 0))
        (test-gemm "gemm-6" blis-dgemm! BLIS-TRANSPOSE BLIS-NO-TRANSPOSE 1. C B 1. (transpose-array A 1 0)))

      (define (with-matrix-types types-AB types-C)
        (for-each
            (match-lambda ((make-A make-B make-C transA transB)
                           (test-gemm (format #f "gemm:~a:~a:~a:~a:~a:~a" type (procedure-name make-A)
                                              (procedure-name make-B) (procedure-name make-C)
                                              transA transB)
                                      gemm! transA transB 3. (fill-A2! (make-A type))
                                      (fill-A2! (make-B type)) 2. (fill-A2! (make-C type)))))
          (apply list-product
            (append (list types-AB types-AB types-C)
                    (make-list 2 (list BLIS-TRANSPOSE BLIS-NO-TRANSPOSE
                                       BLIS-CONJ-NO-TRANSPOSE BLIS-CONJ-TRANSPOSE))))))

      (define with-overlap (list make-M-z1 make-M-z1 make-M-z00 make-M-overlap make-M-overlap-reversed))
      (define without-overlap (list make-M-c-order make-M-fortran-order make-M-offset
                                    make-M-strided make-M-strided-both make-M-strided-reversed))

      (blis-error-checking-level-set! BLIS_NO_ERROR_CHECKING)
      (test-begin "overlap")
      (with-matrix-types with-overlap without-overlap)
      (test-end "overlap")

      (blis-error-checking-level-set! BLIS_FULL_ERROR_CHECKING)
      (test-begin "no overlap")
      (with-matrix-types without-overlap without-overlap)
      (test-end "no overlap")
      ))
 `((f32 ,blis-sgemm!)
   (f64 ,blis-dgemm!)
   (c32 ,blis-cgemm!)
   (c64 ,blis-zgemm!)))


; ---------------------------------
; ?gemv
; ---------------------------------

(define (test-gemv tag gemv! transA conjX alpha A X beta Y)

  ;; alpha*sum_j(A_{ij} * X_j) + beta*Y_i -> Y_i
  (define (ref-gemv! transA conjX alpha A X beta Y)
    (let* ((A (apply-transpose-flag A transA))
           (X (apply-transpose-flag X conjX)))
      (match (array-dimensions A)
        ((M N)
         (do ((i 0 (+ i 1))) ((= i M))
           (array-set! Y (* beta (array-ref Y i)) i)
           (do ((j 0 (+ j 1))) ((= j N))
             (array-set! Y (+ (array-ref Y i) (* alpha (array-ref A i j) (array-ref X j))) i)))
         Y))))

  (let ((Y1 (array-copy Y))
        (Y2 (array-copy Y))
        (AA (array-copy A))
        (XX (array-copy X)))
    (gemv! transA conjX alpha A X beta Y1)
    (ref-gemv! transA conjX alpha A X beta Y2)
    ;; (test-approximate-array tag Y1 Y2 1e-15) ; TODO as a single test.
    (test-begin tag)
    (test-equal Y1 Y2)
    (test-equal AA A)
    (test-equal XX X)
    (test-end tag)))

(for-each
    (match-lambda
      ((type gemv!)
; TODO some extra tests with non-square matrices.
       (define (with-types M-types v1-types v2-types)
         (for-each
             (match-lambda ((make-A make-X make-Y transA conjX)
                            (test-gemv (format #f "gemv:~a:~a:~a:~a:~a:~a" type (procedure-name make-A)
                                               (procedure-name make-X) (procedure-name make-Y)
                                               transA conjX)
                                       gemv! transA conjX 3. (fill-A2! (make-A type))
                                       (fill-A1! (make-X type)) 2. (fill-A1! (make-Y type)))))

           (apply list-product
             (list M-types v1-types v2-types
                   (list BLIS-TRANSPOSE BLIS-NO-TRANSPOSE BLIS-CONJ-NO-TRANSPOSE BLIS-CONJ-TRANSPOSE)
                   (list BLIS-NO-CONJUGATE BLIS-CONJUGATE)))))

       (define with-overlap-M (list make-M-z1 make-M-z1 make-M-z00 make-M-overlap make-M-overlap-reversed))
       (define with-overlap-v (list make-v-z))
       (define without-overlap-v (list make-v-compact make-v-strided make-v-offset make-v-strided-reversed))
       (define without-overlap-M (list make-M-c-order make-M-fortran-order make-M-offset
                                       make-M-strided make-M-strided-both make-M-strided-reversed))

       (blis-error-checking-level-set! BLIS_FULL_ERROR_CHECKING)
       (with-types with-overlap-M without-overlap-v without-overlap-v)
       (with-types without-overlap-M with-overlap-v without-overlap-v)
       (blis-error-checking-level-set! BLIS_FULL_ERROR_CHECKING)
       (with-types without-overlap-M without-overlap-v without-overlap-v)))
  `((f32 ,blis-sgemv!)
    (f64 ,blis-dgemv!)
    (c32 ,blis-cgemv!)
    (c64 ,blis-zgemv!)))


; ---------------------------------
; ?ger
; ---------------------------------

(define (test-ger tag ger! conjX conjY alpha X Y A)

  ;; alpha*x_i*y_j + A_{i, j} -> A_{i, j}
  (define (ref-ger! conjX conjY alpha X Y A)
    (let* ((X (apply-transpose-flag X conjX))
           (Y (apply-transpose-flag Y conjY))
           (M (array-length X))
           (N (array-length Y)))
      (match (array-dimensions A)
        ((M N)
         (do ((i 0 (+ i 1))) ((= i M))
           (do ((j 0 (+ j 1))) ((= j N))
             (array-set! A (+ (array-ref A i j) (* alpha (array-ref X i) (array-ref Y j))) i j)))
         Y))))

  (let ((A1 (array-copy A))
        (A2 (array-copy A)))
    (ger! conjX conjY alpha X Y A1)
    (ref-ger! conjX conjY alpha X Y A2)
    ;; (test-approximate-array tag A1 A2 1e-15) ; TODO as a single test.
    (test-begin tag)
    (test-equal A1 A2)
    (test-end tag)))

(for-each
    (match-lambda
      ((type ger!)
; TODO some extra tests with non-square matrices.
       (for-each
           (match-lambda ((make-X make-Y make-A conjX conjY)
                          (test-ger (format #f "ger:~a:~a:~a:~a:~a:~a" type (procedure-name make-X)
                                            (procedure-name make-Y) (procedure-name make-A)
                                            conjX conjY)
                                    ger! conjX conjY 3.
                                    (fill-A1! (make-X type))
                                    (fill-A1! (make-Y type))
                                    (fill-A2! (make-A type)))))
         (list-product
          (list make-v-compact make-v-strided make-v-offset make-v-strided-reversed)
          (list make-v-compact make-v-strided make-v-offset make-v-strided-reversed)
          (list make-M-c-order make-M-fortran-order make-M-offset
                make-M-strided make-M-strided-both make-M-strided-reversed)
          (list BLIS-NO-CONJUGATE BLIS-CONJUGATE)
          (list BLIS-NO-CONJUGATE BLIS-CONJUGATE)))))
  `((f32 ,blis-sger!)
    (f64 ,blis-dger!)
    (c32 ,blis-cger!)
    (c64 ,blis-zger!)))

(define error-count (test-runner-fail-count (test-runner-current)))
(test-end "ffi-blis")
(exit error-count)
