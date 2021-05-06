#lang racket

(provide (all-from-out "types.rkt")
         (all-defined-out))

(require compatibility/mlist)

(require "utilities.rkt")

(require "types.rkt")

(define void-v? void?)
(define make-void-v void)
(define void-v-value identity)
(define-instance fprint-v void-v?
    (λ (v port) (display '|#<void>| port)))

(define null-v? null?)
(define make-null-v (λ () null))
(define null-v-value identity)
; fprint-v implemented as list-v

(define bool-v? boolean?)
(define (make-bool-v x)
    (unless (boolean? x)
        (error 'make-bool-v "invalid value"))
    x)
(define bool-v-value identity)
(define-instance fprint-v bool-v?
    (λ (v port) (display v port)))

(define number-v? number?)
(define (make-number-v x)
    (unless (number? x)
        (error 'make-number-v "invalid value"))
    x)
(define number-v-value identity)
(define-instance fprint-v number-v?
    (λ (v port) (display v port)))

(define string-v? string?)
(define (make-string-v x)
    (unless (string? x)
        (error 'make-string-v "invalid value"))
    x)
(define string-v-value identity)
(define-instance fprint-v string-v?
    (λ (v port) (display v port)))

(define symbol-v? symbol?)
(define (make-symbol-v x)
    (unless (symbol? x)
        (error 'make-symbol-v "invalid-value"))
    x)
(define symbol-v-value identity)
(define-instance fprint-v symbol-v?
    (λ (v port) (display v port)))

(define list-v? mlist?)
(define make-list-v list->mlist)
(define list-v-value mlist->list)
(define-instance fprint-v list-v?
    (λ (v port)
        (define (print-iter v)
            (unless (null-v? v)
                (display #\, port)
                (display #\space port)
                (fprint-v (pair-v-car v) port)
                (print-iter (pair-v-cdr v))))
        (display #\[ port)
        (unless (null? v)
            (fprint-v (pair-v-car v) port)
            (print-iter (pair-v-cdr v)))
        (display #\] port)))

(define pair-v? mpair?)
(define make-pair-v mcons)
(define pair-v-car mcar)
(define pair-v-cdr mcdr)
(define set-pair-v-car! set-mcar!)
(define set-pair-v-cdr! set-mcdr!)
(define-instance fprint-v pair-v?
    (λ (v port)
        (display #\( port)
        (display 'cons port)
        (display #\space port)
        (fprint-v (pair-v-car v) port)
        (display #\, port)
        (fprint-v (pair-v-cdr v) port)
        (display #\) port)))

(struct-with-make proc-v (name proc)
    #:guard
    (λ (name proc name*)
        (if (and ((maybe/c symbol?) name)
                 (procedure? proc))
            (values name proc)
            (error 'make-proc-v "invalid value"))))
(define-instance fprint-v proc-v?
    (λ (v port)
        (display '|#<procedure| port)
        (match (proc-v-name v)
            [(just name) (display #\space port)
                         (display name)]
            [(nothing) (void)])
        (display #\> port)))
(define-instance apply-v proc-v?
    (λ (proc params)
        (apply (proc-v-proc proc) params)))
