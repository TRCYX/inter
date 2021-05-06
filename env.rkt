#lang racket

(provide the-empty-frame
         (struct-out frame)
         the-empty-env
         env?
         extend-env
         lookup-var
         set-var!
         define-var!
         the-builtin-frame the-builtin-env
         the-global-frame the-global-env)

(require racket/struct)

(require "utilities.rkt")

;;; Frame
(struct frame (bindings)
    #:constructor-name make-frame
    #:guard
    (λ (bindings name)
        (cond [(hash? bindings) (hash-copy bindings)]
              [((listof (cons/c symbol? any/c)) bindings) (make-hasheq bindings)]
              [else (error 'make-frame "invalid argument")]))
    #:methods gen:custom-write
    [(define write-proc
        (make-constructor-style-printer
            (const 'frame)
            (compose1 list (λ (f) (frame-bindings f)))))])

(define the-empty-frame (make-frame null))
(define (frame-try-ref frame key)
    (let ([result (hash-ref (frame-bindings frame) key (nothing))])
         (if (nothing? result)
             (nothing)
             (just result))))
(define (frame-add-or-update-binding! frame key value)
    (hash-set! (frame-bindings frame) key value))
(define (frame-try-set-binding! frame key value) ; Returns #t if succeeds; #f otherwise
    (with-handlers ([exn:fail:contract? (const #f)])
        (hash-update! (frame-bindings frame) key (const value)
        #t)))

;;; Env
(define env? (listof frame?))
(define the-empty-env null)
(define (first-frame env) (car env))
(define (rest-frames env) (cdr env))
(define (extend-env env frame) (cons frame env))
(define (lookup-var env var)
    (if (null? env)
        (error "no such variable:" var)
        (let ([result (frame-try-ref (first-frame env) var)])
             (match result
                 [(just x) x]
                 [(nothing) (lookup-var (rest-frames env) var)]))))
(define (set-var! env var value)
    (if (null? env)
        (error "no such variable:" var)
        (unless (frame-try-set-binding! (first-frame env) var value)
            (set-var! (rest-frames env) var value))))
(define (define-var! env var value)
    (if (null? env)
        (error "defining" var "in empty environment")
        (frame-add-or-update-binding! (first-frame env) var value)))

(define the-builtin-frame (make-frame null))
(define the-builtin-env
    (extend-env the-empty-env the-builtin-frame))
(define the-global-frame (make-frame null))
(define the-global-env
    (extend-env the-builtin-env the-global-frame))
