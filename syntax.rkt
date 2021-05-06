#lang racket

(provide (all-from-out "eval.rkt")
         (except-out (all-defined-out)
                     tagged-list?))

(require "utilities.rkt")

(require "env.rkt")
(require "eval.rkt")
(require "values.rkt")

(define (tagged-list? tag exp)
    (and (list? exp)
         (not (null? exp))
         (eq? tag (first exp))))

(define (improper-list-of pred)
    (define (check x)
        (or (null? x)
            (pred x)
            (and (pair? x)
                 (pred (car x))
                 (check (cdr x)))))
    check)

(define (bool-true? x) (not (bool-false? x)))
(define (bool-false? x) (and (bool-v? x) (not (bool-v-value x))))

(define (bool? exp)
    (or (eq? exp #t) (eq? exp #f)))
(define (analyze-bool exp)
    (const (make-bool-v exp)))
(add-syntax! (make-sub-syntax 'bool bool? analyze-bool))

(define (analyze-number exp)
    (const (make-number-v exp)))
(add-syntax! (make-sub-syntax 'number number? analyze-number))

(define (analyze-string exp)
    (const (make-string-v exp)))
(add-syntax! (make-sub-syntax 'string string? analyze-string))

(define (cadr? exp)
    (and (symbol? exp)
         (regexp-match? #rx"^c[ad]+r$" (symbol->string exp))))
(define (analyze-cadr exp)
    (define (perform-cadr ops v)
        (cond [(null? ops) v]
              [(not (pair-v? v)) (error exp "non-pair value encountered")]
              [(eq? (first ops) #\a) (perform-cadr (rest ops) (pair-v-car v))]
              [else                  (perform-cadr (rest ops) (pair-v-cdr v))]))
    (define (to-car/cdr a/d)
        (if (eq? a/d #\a) pair-v-car pair-v-cdr))
    (let* ([operator (symbol->string exp)]
           [a/ds (substring operator 1 (sub1 (string-length operator)))])
        (const
            (make-proc-v (just exp)
                (apply compose1 (map to-car/cdr (string->list a/ds)))))))
(add-syntax! (make-sub-syntax 'cadr cadr? analyze-cadr))

(define (variable? exp) (symbol? exp))
(define ((analyze-variable exp) env) (lookup-var env exp))
(add-syntax! (make-sub-syntax 'variable variable? analyze-variable))

(define (set!? exp) (tagged-list? 'set! exp))
(define (set-car!? exp) (tagged-list? 'set-car! exp))
(define (set-cdr!? exp) (tagged-list? 'set-cdr! exp))
(define (set!-type exp) (car exp))
(define (set!-var exp) (cadr exp))
(define (set!-value exp) (caddr exp))
(define (check-set! exp)
    (unless (= (length exp) 3)
        (error (set!-type exp) "invalid number of parts"))
    (unless (symbol? (set!-var exp))
        (error (set!-type exp) "expected variable")))
(define (analyze-set! exp)
    (check-set! exp)
    (let ([var (set!-var exp)]
          [value-analyzed (analyze (set!-value exp))])
         (cond [(set!? exp)
                (λ (env)
                    (set-var! env var (value-analyzed env))
                    (make-void-v))]
               [(set-car!? exp)
                (λ (env)
                    (let ([var-value (lookup-var env var)])
                         (unless (pair-v? var-value)
                             (error 'set-car! "not a pair"))
                         (set-pair-v-car! var-value (value-analyzed env))
                         (make-void-v)))]
               [(set-cdr!? exp)
                (λ (env)
                    (let ([var-value (lookup-var env var)])
                         (unless (pair-v? var-value)
                             (error 'set-cdr! "not a pair"))
                         (set-pair-v-cdr! var-value (value-analyzed env))
                         (make-void-v)))])))
(add-syntax! (make-sub-syntax 'set! set!? analyze-set!))
(add-syntax! (make-sub-syntax 'set-car! set-car!? analyze-set!))
(add-syntax! (make-sub-syntax 'set-cdr! set-cdr!? analyze-set!))

(define (quoted? exp) (tagged-list? 'quote exp))
(define (quoted-exp exp) (cadr exp))
(define (analyze-quoted exp)
    (define (to-quote-v exp)
        (cond [(bool? exp) ((analyze-bool exp) the-empty-env)]
              [(number? exp) ((analyze-number exp) the-empty-env)]
              [(string? exp) ((analyze-string exp) the-empty-env)]
              [(symbol? exp) (make-symbol-v exp)]
              [(list? exp) (make-list-v (map to-quote-v exp))]
              [else (error 'quote "wrong syntax")]))
    (unless (and (not (null? (cdr exp))) (null? (cddr exp)))
        (error 'quote "wrong syntax"))
    (const (to-quote-v (quoted-exp exp))))
(add-syntax! (make-sub-syntax 'quote quoted? analyze-quoted))

(define (definition? exp)
    (and (tagged-list? 'define exp)
         (>= (length exp) 3)))

(define (vardef? exp)
    (and (definition? exp)
         (symbol? (cadr exp))))
(define (vardef-var exp) (cadr exp))
(define (vardef-sequence exp) (cddr exp))
(define (analyze-vardef exp)
    (let ([var (vardef-var exp)]
          [value-analyzed (analyze-sequence (vardef-sequence exp))])
         (λ (env)
            (define-var! env var (value-analyzed (extend-env env (make-frame null))))
            (make-void-v))))

(define (funcdef? exp)
    (and (definition? exp)
         (pair? (cadr exp))))
(define (funcdef-var exp) (caadr exp))
(define (funcdef-params exp) (cdadr exp))
(define (funcdef-body exp) (cddr exp))
(define (analyze-funcdef exp)
    (unless ((improper-list-of symbol?) (funcdef-params exp))
        (error 'define "wrong syntax"))
    (let ([var (funcdef-var exp)]
          [value-analyzed (analyze-lambda (make-lambda (funcdef-params exp) (funcdef-body exp)))])
         (λ (env)
            (define-var! env var (value-analyzed env))
            (make-void-v))))

(add-syntax!
    (make-sub-syntax-from-syntaxes
        'define
        definition?
        (list (make-sub-syntax 'vardef vardef? analyze-vardef)
              (make-sub-syntax 'funcdef funcdef? analyze-funcdef))))

(define (if? exp) (tagged-list? 'if exp))
(define (if-cond exp) (cadr exp))
(define (if-then exp) (caddr exp))
(define (if-else exp) (cadddr exp))
(define (analyze-if exp)
    (unless (let ([len (length exp)]) (or (= len 3) (= len 4)))
        (error 'if "wrong syntax"))
    (let ([cond-analyzed (analyze (if-cond exp))]
          [then-analyzed (analyze (if-then exp))])
         (if (= (length exp) 3)
             (λ (env)
                (if (bool-true? (cond-analyzed env))
                    (then-analyzed env)
                    (make-void-v)))
             (let ([else-analyzed (analyze (if-else exp))])
                  (λ (env)
                     (if (bool-true? (cond-analyzed env))
                         (then-analyzed env)
                         (else-analyzed env)))))))
(add-syntax! (make-sub-syntax 'if if? analyze-if))

(define (and? exp) (tagged-list? 'and exp))
(define (and-conds exp) (cdr exp))
(define (analyze-and exp)
    (define ((and-concat first-analyzed rest-analyzed) env)
        (let ([first-res (first-analyzed env)])
             (if (bool-false? first-res)
                 first-res
                 (rest-analyzed env))))
    (let ([conds (and-conds exp)])
         (if (null? conds)
             (const (make-bool-v #t))
             (foldr1 and-concat (map analyze conds)))))
(add-syntax! (make-sub-syntax 'and and? analyze-and))

(define (or? exp) (tagged-list? 'or exp))
(define (or-conds exp) (cdr exp))
(define (analyze-or exp)
    (define ((or-concat first-analyzed rest-analyzed) env)
        (let ([first-res (first-analyzed env)])
             (if (bool-true? first-res)
                 first-res
                 (rest-analyzed env))))
    (let ([conds (or-conds exp)])
         (if (null? conds)
             (const (make-bool-v #f))
             (foldr1 or-concat (map analyze conds)))))
(add-syntax! (make-sub-syntax 'or or? analyze-or))

(define (cond? exp) (tagged-list? 'cond exp))
(define (cond-clauses exp) (cdr exp))
(define (cond-clause-cond clause) (car clause))
(define (cond-clause-body clause) (cdr clause))
(define (analyze-cond exp)
    (define (check-clause clause)
        (cond [(not (list? clause)) (error "cond clause not list")]
              [(< (length clause) 2) (error "not enough parts in cond clause")]))
    (foldr (λ (first-clause rest-analyzed)
              (check-clause first-clause)
              (let ([cond* (cond-clause-cond first-clause)]
                    [body-analyzed (analyze-sequence (cond-clause-body first-clause))])
                   (if (eq? cond* 'else)
                       body-analyzed
                       (let ([cond-analyzed (analyze (cond-clause-cond first-clause))])
                            (λ (env)
                                (if (bool-true? (cond-analyzed env))
                                    (body-analyzed env)
                                    (rest-analyzed env)))))))
           (const (make-void-v))
           (cond-clauses exp)))
(add-syntax! (make-sub-syntax 'cond cond? analyze-cond))

(define (begin? exp) (tagged-list? 'begin exp))
(define (begin-body exp) (cdr exp))
(define (analyze-begin exp)
    (analyze-sequence (begin-body exp)))
(add-syntax! (make-sub-syntax 'begin begin? analyze-begin))

(define (lambda? exp)
    (or (tagged-list? 'lambda exp)
        (tagged-list? 'λ exp)))
(define (make-lambda params body) (append (list 'lambda params) body))
(define (lambda-formals exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (analyze-lambda exp)
    (unless (and (>= (length exp) 3) ((improper-list-of symbol?) (lambda-formals exp)))
        (error 'lambda "wrong syntax"))
    (let ([body-analyzed (analyze-sequence (lambda-body exp))]
          [formals (lambda-formals exp)])
         (λ (env)
            (let ([proc
                   (λ actuals
                      (let ([extended-env
                             (extend-env env
                                         (formals-actuals-frame formals actuals))])
                           (body-analyzed extended-env)))])
                 (make-proc-v (nothing) proc)))))
(add-syntax! (make-sub-syntax 'lambda lambda? analyze-lambda))

(define (let? exp) (tagged-list? 'let exp))
(define (let-var-list exp) (cadr exp))
(define (let-vars exp) (map car (let-var-list exp)))
(define (let-vals exp) (map cadr (let-var-list exp)))
(define (let-body exp) (cddr exp))
(define (analyze-let exp)
    (define (check-let exp)
        (when (< (length exp) 2)
            (error 'let "two few parts"))
        (unless ((listof (cons/c symbol? (cons/c any/c null?))) (let-var-list exp))
            (error 'let "wrong syntax")))
    (check-let exp)
    (analyze-apply (make-apply (make-lambda (let-vars exp) (let-body exp))
                               (let-vals exp))))
(add-syntax! (make-sub-syntax 'let let? analyze-let))

(define (apply? exp) (and (list? exp) (not (null? exp))))
(define (make-apply func args) (cons func args))
(define (apply-func exp) (car exp))
(define (apply-args exp) (cdr exp))
(define (analyze-apply exp)
    (let ([func-analyzed (analyze (apply-func exp))]
          [args-analyzed (map analyze (apply-args exp))])
         (λ (env)
            (let ([func (func-analyzed env)]
                  [args (map (λ (f) (f env)) args-analyzed)])
                 (apply-v func args)))))
(add-syntax! (make-sub-syntax 'apply apply? analyze-apply))

(define (formals-actuals-frame formals actuals)
    (define (formal-actual-pairs formals actuals)
        (cond [(null? formals)
               (if (null? actuals) null (error "too many arguments"))]
              [(symbol? formals)
               (list (cons formals (make-list-v actuals)))]
              [((cons/c symbol? any/c) formals)
               (cons (cons (car formals) (car actuals))
                     (formal-actual-pairs (cdr formals) (cdr actuals)))]
              [else (error "invalid procedure application")]))
    (make-frame (formal-actual-pairs formals actuals)))
