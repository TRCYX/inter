#lang racket

(require racket/cmdline)

(require "builtin.rkt")
(require "inter.rkt")
(require "syntax.rkt")
(require "types.rkt")
(require "values.rkt")

(define (main)
    (define interactive (make-parameter #f))
    (define source-names
        (command-line
            #:program "Interpreter"
            #:once-each
            [("-i" "--interactive") "Interactive mode"
                                    (interactive #t)]
            #:args file-names
            file-names))

    (for ([source-name source-names])
        (load-file source-name))

    (when (interactive)
          (define (main-loop)
              (display ">>> ")
              (let ([code (read)])
                   (print-v (interpret code))
                   (newline)
                   (main-loop)))
          (main-loop)))

(time (main))
