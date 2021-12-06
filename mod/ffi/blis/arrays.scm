
; Helper functions for BLIS bindings.
; (c) Daniel Llorens - 2014-2015, 2017, 2019, 2021

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU Lesser General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

; This file is shared with (ffi cblas arrays) in guile-ffi-cblas.

(define-module (ffi blis arrays)
  #:export (syntax->list
            check-array check-2-arrays
            stride dim
            define-sdcz
            define-auto))

(import (system foreign) (srfi srfi-1) (srfi srfi-11) (srfi srfi-26) (ice-9 match))

(define (stride A i)
  (list-ref (shared-array-increments A) i))

(define (dim A i)
  (list-ref (array-dimensions A) i))

(define (check-array A rank type)
  (unless (= rank (array-rank A)) (throw 'bad-rank (array-rank A)))
  (unless (typed-array? A type) (throw 'bad-type type (array-type A))))

; FIXME return shapes, since those are often neede after the check
(define (check-2-arrays A B rank type)
  (check-array A rank type)
  (check-array B rank type)
  (let ((sha (array-shape A))
        (shb (array-shape B)))
    (for-each (match-lambda*
                (((alo ahi) (blo bhi))
                 (unless (= 0 alo blo) (throw 'bad-base-indices sha shb))
                 (unless (= ahi bhi) (throw 'mismatched-sizes sha shb))))
      sha shb)))

; https://www.scheme.com/csug8/syntax.html §11.3
(define syntax->list
  (lambda (ls)
    (syntax-case ls ()
      (() '())
      ((x . r) (cons #'x (syntax->list #'r))))))

(eval-when (expand load eval)
  (define (subst-qmark stx-name t)
    (let* ((s (symbol->string (syntax->datum stx-name)))
           (i (string-index s #\?)))
      (datum->syntax stx-name (string->symbol (string-replace s (symbol->string t) i (+ i 1)))))))

(define-syntax define-sdcz
  (lambda (x)
    (syntax-case x ()
      ((_ root n ...)
       (with-syntax ((definer (datum->syntax x (string->symbol (format #f "define-~a" (syntax->datum #'root))))))
         (cons #'begin
               (append-map
                (lambda (tag t)
                  (let ((fun (map (cut subst-qmark <> t) (syntax->list #'(n ...)))))
; #`(quote #,(datum->syntax x tag)) to write out a symbol, but assembling docstrings seems harder (?)
                    (list (cons* #'definer (datum->syntax x tag) fun)
                          (cons* #'export fun))))
                '(f32 f64 c32 c64)
                '(s d c z))))))))

(define-syntax define-auto
  (lambda (x)
    (syntax-case x ()
      ((_ (fun args ...) X ?fun)
       #`(begin
           (define (fun args ...)
             ((match (array-type X)
                #,@(map (lambda (tag t) (list #`(quote #,(datum->syntax x tag)) (subst-qmark #'?fun t)))
                     '(f32 f64 c32 c64)
                     '(s d c z)))
              args ...))
           (export fun))))))
