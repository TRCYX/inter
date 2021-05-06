#lang racket

(provide (all-defined-out))

(require "utilities.rkt")

(define-interface (apply-v proc params))
(define-interface (fprint-v value port))

(define (print-v v) (fprint-v v (current-output-port)))
(define (sprint-v v)
    (let ([s-port (open-output-string)])
         (fprint-v v s-port)
         (get-output-string s-port)))
(define (print-values . values)
    (when (not (null? values))
        (print-v (first values))
        (for ([value (rest values)])
            (display " ")
            (print-v value)))
    (newline))
