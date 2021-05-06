#lang racket

(provide (struct-out sub-syntax)
         make-sub-syntax-from-syntaxes
         add-syntax!
         analyze
         analyze-sequence
         evaluate)

(require "utilities.rkt")

(require "env.rkt")
(require "values.rkt")

(struct-with-make sub-syntax (name pred analyze)
    #:guard
    (λ (name pred analyze name*)
        (unless (symbol? name)
            (error 'make-sub-syntax "name not a symbol"))
        (unless (procedure? pred)
            (error 'make-sub-syntax "pred not a procedure"))
        (unless (procedure? analyze)
            (error 'make-sub-syntax "analyze not a procedure"))
        (values name pred analyze)))

(define ((analyze-with-syntaxes name syntaxes) exp)
    (define (analyze-iter syntaxes)
        (if (null? syntaxes)
            (error name "wrong syntax")
            (let ([first-syntax (first syntaxes)]
                  [rest-syntaxes (rest syntaxes)])
                 (let ([pred (sub-syntax-pred first-syntax)]
                       [analyze (sub-syntax-analyze first-syntax)])
                      (if (pred exp)
                          (analyze exp)
                          (analyze-iter rest-syntaxes))))))
    (analyze-iter syntaxes))

(define (make-sub-syntax-from-syntaxes name pred syntaxes)
    (make-sub-syntax name pred (analyze-with-syntaxes name syntaxes)))

(define all-syntaxes null)
(define (add-syntax! syntax)
    (set! all-syntaxes (append all-syntaxes (list syntax))))

(define (analyze exp)
    ((analyze-with-syntaxes 'eval all-syntaxes) exp))

(define (analyze-sequence exps)
    (cond [(not (list? exps))
           (error 'eval-sequence "exps is not list?")]
          [(null? exps)
           (λ (env) (make-void-v))]
          [else
           (let ([analyzed (map analyze exps)])
                (foldl1 (λ (f acc) (λ (env) (acc env) (f env))) analyzed))]))

(define (evaluate exp env)
    ((analyze exp) the-global-env))
