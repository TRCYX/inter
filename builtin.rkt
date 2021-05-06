#lang racket

(require "utilities.rkt")

(require "env.rkt")
(require "eval.rkt")
(require "inter.rkt")
(require "syntax.rkt")
(require "values.rkt")

(define (define-builtin! var value)
    (define-var! the-builtin-env var value))

(define (define-primitive-proc! name proc)
    (define-builtin! name (make-proc-v (just name) proc)))

(define-primitive-proc! 'exit exit)
(define-primitive-proc! 'print print-values)

(define-primitive-proc! 'void make-void-v)

(define-primitive-proc! 'null? (compose1 make-bool-v null-v?))

(define-primitive-proc! 'bool? (compose1 make-bool-v bool-v?))
(define-primitive-proc! 'number? (compose1 make-bool-v number-v?))
(define-primitive-proc! 'string? (compose1 make-bool-v string-v?))
(define-primitive-proc! 'symbol? (compose1 make-bool-v symbol-v?))

(define-builtin! 'true (make-bool-v #t))
(define-builtin! 'false (make-bool-v #f))

(define-primitive-proc! 'not
    (λ (x) (make-bool-v (bool-false? x))))

(define (all-equal? eq xs)
    (define (all-equal?-iter x ys)
        (cond [(null? ys) #t]
              [(not (eq x (first ys))) #f]
              [else (all-equal?-iter x (rest ys))]))
    (if (null? xs)
        #t
        (all-equal?-iter (first xs) (rest xs))))
(define-primitive-proc! 'eq?
    (λ xs (make-bool-v (all-equal? eq? xs))))
(define-primitive-proc! 'not-eq?
    (λ xs (make-bool-v (not (all-equal? eq? xs)))))
(define-primitive-proc! 'equal?
    (λ xs (make-bool-v (all-equal? equal? xs))))
(define-primitive-proc! 'not-equal?
    (λ xs (make-bool-v (not (all-equal? equal? xs)))))

(define-primitive-proc! '+
    (λ numbers (make-number-v (apply + (map number-v-value numbers)))))
(define-primitive-proc! '-
    (λ numbers (make-number-v (apply - (map number-v-value numbers)))))
(define-primitive-proc! '*
    (λ numbers (make-number-v (apply * (map number-v-value numbers)))))
(define-primitive-proc! '/
    (λ numbers (make-number-v (apply / (map number-v-value numbers)))))
(define-primitive-proc! '%
    (λ numbers
        (let ([values (map number-v-value numbers)])
            (let ([dividend (first values)]
                  [divisors (rest values)])
                 (make-number-v (foldl (λ (x y) (remainder y x)) dividend divisors))))))
(define-primitive-proc! '**
    (λ (base exp) (make-number-v (expt (number-v-value base) (number-v-value exp)))))

(define-primitive-proc! '=
    (λ numbers (make-bool-v (apply = (map number-v-value numbers)))))
(define-primitive-proc! '!=
    (λ numbers (make-bool-v (not (apply = (map number-v-value numbers))))))
(define-primitive-proc! '<
    (λ numbers (make-bool-v (apply < (map number-v-value numbers)))))
(define-primitive-proc! '>
    (λ numbers (make-bool-v (apply > (map number-v-value numbers)))))
(define-primitive-proc! '<=
    (λ numbers (make-bool-v (apply <= (map number-v-value numbers)))))
(define-primitive-proc! '>=
    (λ numbers (make-bool-v (apply >= (map number-v-value numbers)))))

(define-primitive-proc! 'cons make-pair-v)
(define-primitive-proc! 'pair? (compose1 make-bool-v pair-v?))

(define-primitive-proc! 'apply
    (λ (proc . vs)
        (define (to-raw-list vs)
            (if (null? (rest vs))
                (list-v-value (first vs))
                (cons (first vs) (to-raw-list (rest vs)))))
        (apply-v proc (to-raw-list vs))))

(define-primitive-proc! 'load
    (λ file-names
        (for ([file-name file-names])
            (if (string-v? file-name)
                (load-file (string-v-value file-name))
                (error 'load (format "file name ~a not a string" (sprint-v file-name)))))))

(define-builtin! 'null (make-null-v))

(define-primitive-proc! 'error
    (λ (arg . args)
        (cond [(and (string-v? arg)
                    (null? args))
               (error (string-v-value arg))]
              [(and (symbol-v? arg)
                    (= (length args) 1))
               (error (symbol-v-value arg) (sprint-v (first args)))]
              [else (error 'error "invalid arguments")])))

(define-primitive-proc! 'list
    (λ args (make-list-v args)))

(define-primitive-proc! 'reverse
    (λ (l) (foldl-v make-pair-v (make-null-v) l)))

(define-primitive-proc! 'append
    (λ lists
        (define (concat first-list second-list)
            (define (iter first-list)
                (if (null-v? first-list)
                    second-list
                    (make-pair-v (pair-v-car first-list) (iter (pair-v-cdr first-list)))))
            (iter first-list))
        (foldr concat null lists)))
(define-primitive-proc! 'map
    (λ (proc . lists)
        (when (null? lists)
            (error 'map "no lists given"))
        (define (do-map lists)
            (if (null-v? (first lists))
                (make-null-v)
                (make-pair-v (apply-v proc (map pair-v-car lists))
                             (do-map (map pair-v-cdr lists)))))
        (do-map lists)))
(define (filter-v pred xs)
    (cond [(null-v? xs) (make-null-v)]
          [(apply-v pred (list (pair-v-car xs)))
           (make-pair-v (pair-v-car xs)
                        (filter-v pred (pair-v-cdr xs)))]
          [else (filter-v pred (pair-v-cdr xs))]))
(define-primitive-proc! 'filter filter-v)
(define (foldl-v proc init xs)
    (if (null-v? xs)
        init
        (foldl-v proc (apply-v proc (list init (pair-v-car xs))) (pair-v-cdr xs))))
(define-primitive-proc! 'foldl foldl-v)
(define (foldr-v proc init xs)
    (if (null-v? xs)
        init
        (apply-v proc
                 (list (pair-v-car xs)
                       (foldr-v proc init (pair-v-cdr xs))))))
(define-primitive-proc! 'foldr foldr-v)
(define (foldl1-v proc xs)
    (if (not (list-v? xs))
        (error 'foldl1 "empty list given")
        (foldl-v proc (pair-v-car xs) (pair-v-cdr xs))))
(define-primitive-proc! 'foldl1 foldl1-v)
(define (foldr1-v proc xs)
    (define (do-foldr1 xs)
        (if (null-v? (pair-v-cdr xs))
            (pair-v-car xs)
            (apply-v proc (list (pair-v-car xs)
                                (do-foldr1 (pair-v-cdr xs))))))
    (if (not (list-v? xs))
        (error 'foldr1 "empty list given")
        (do-foldr1 xs)))
(define-primitive-proc! 'foldr1 foldr1-v)

(define (app-v f x) (apply-v f (list x)))
(define-primitive-proc! 'app app-v)
(define-primitive-proc! 'flip
    (λ (f) (make-proc-v (nothing) (λ (x y) (apply-v f (list y x))))))
(define-primitive-proc! 'compose
    (λ procs (make-proc-v (nothing) (λ (x) (foldr app-v x procs)))))

(run-code the-builtin-env
    (define remainder %)
    (define (abs x) (if (< x 0) (- x) x))
    (define (add1 x) (+ x 1))
    (define (sub1 x) (- x 1))

    (define first car)
    (define rest cdr)
    (define second cadr)
    (define third caddr)
    (define fourth cadddr)
    (define fifth caddddr)
    (define sixth cadddddr)
    (define seventh caddddddr)
    (define eighth cadddddddr)
    (define ninth caddddddddr)
    (define tenth cadddddddddr)

    (define (Y f) (f (λ xs (apply (Y f) xs))))
)
