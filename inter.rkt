#lang racket

(provide interpret
         run-code
         load-file)

(require "eval.rkt")
(require "env.rkt")

(define (interpret code [env the-global-env])
    (with-handlers ([exn:fail? (λ (e) (eprintf "error: ~a\n" (exn-message e)))])
        (evaluate code env)))

(define-syntax-rule (run-code env code ...)
    (void (interpret 'code env) ...))

(define (load-file file-name)
    (with-handlers ([exn:fail:filesystem?
                     (λ (e) (eprintf "Can't open ~a\n" file-name))])
        (define source (open-input-file file-name #:mode 'text))
        (define (read-all)
            (let ([code (read source)])
                    (when (not (eof-object? code))
                        (begin (interpret code)
                               (read-all)))))
        (read-all)
        (close-input-port source)))
