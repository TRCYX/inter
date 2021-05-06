#lang racket

(provide maybe? maybe/c
         just just?
         nothing nothing?
         maybe-extract maybe-value-or
         foldl1 foldr1
         (for-syntax constructor-name)
         struct-with-make
         define-interface define-instance interface-out)

(require racket/struct)
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))
(require racket/provide-syntax)

(struct maybe ())
(struct just maybe (value)
    #:methods gen:equal+hash
    [(define (equal-proc x y equal?-recur)
        (equal?-recur (just-value x) (just-value y)))
     (define (hash-proc x hash-recur)
        (hash-recur x))
     (define (hash2-proc x hash2-recur)
        (hash2-recur x))]
    #:methods gen:custom-write
    [(define write-proc
        (make-constructor-style-printer
            (lambda (x) 'just)
            (lambda (x) (list (just-value x)))))])
(struct nothing maybe ()
    #:methods gen:equal+hash
    [(define (equal-proc x y equal?-recur) #t)
     (define (hash-proc x hash-recur) 0)
     (define (hash2-proc x hash2-recur) 0)]
    #:methods gen:custom-write
    [(define write-proc
        (make-constructor-style-printer
            (lambda (x) 'nothing)
            (lambda (x) null)))])

(define ((maybe/c contract) x)
    (match x
        [(just x) (contract x)]
        [(nothing) #t]
        [_ #f]))
(define (maybe-extract maybe)
    (match maybe
        [(just x) x]
        [(nothing) (error "not a just?")]))
(define (maybe-value-or maybe alt)
    (match maybe
        [(just x) x]
        [(nothing) alt]))

(define (foldl1 proc xs)
    (cond [(not (pair? xs)) (error 'foldl1 "not a non-empty list")]
          [else (foldl proc (first xs) (rest xs))]))
(define (foldr1 proc xs)
    (cond [(not (pair? xs)) (error 'foldr1 "not a non-empty list")]
          [(null? (rest xs)) (first xs)]
          [else (proc (first xs) (foldr1 proc (rest xs)))]))

(begin-for-syntax
    (define (constructor-name stx)
        (syntax-parse stx
            [type:id
             (format-id #'type "make-~a" #'type)])))
(define-syntax (struct-with-make stx)
   (define-syntax-class struct-members
      #:description "struct members"
      (pattern (members:id ...)))
   (syntax-parse stx
      [(_ type:id (~optional super:id) members:struct-members . rest)
       #:with constructor-name
              (constructor-name #'type)
       #'(struct type (~? super) (members.members ...)
                 #:constructor-name constructor-name
                 . rest)]))

(struct-with-make dispatcher (pred-datum-pairs) #:mutable)
(define (add-dispatch dispatcher pred datum)
    (set-dispatcher-pred-datum-pairs! dispatcher
        (append (dispatcher-pred-datum-pairs dispatcher)
                (list (cons pred datum)))))
(define (dispatch-with-pred-datum-pairs pred-datum-pairs value)
    (if (null? pred-datum-pairs)
        (nothing)
        (let ([pred (caar pred-datum-pairs)]
              [datum (cdar pred-datum-pairs)])
             (if (pred value)
                 (just datum)
                 (dispatch-with-pred-datum-pairs (rest pred-datum-pairs)
                                                 value)))))
(define (dispatch dispatcher value)
    (dispatch-with-pred-datum-pairs (dispatcher-pred-datum-pairs dispatcher)
                                    value))

(begin-for-syntax
    (define (dispatcher-name stx)
        (syntax-parse stx
            [func:id
             (format-id #'func "~a-dispatcher" #'func)])))
(define-syntax (define-interface stx)
    (syntax-parse stx
        [(_ (func:id dispatching-param:id . params)
            (~optional (~seq #:default default-proc:expr)))
         #:with dispatcher-name
                (dispatcher-name #'func)
         #'(begin
            (define dispatcher-name (make-dispatcher null))
            (define (func dispatching-param . params)
                (match (dispatch dispatcher-name dispatching-param)
                    [(just proc) (proc dispatching-param . params)]
                    [(nothing) (~? (default-proc dispatching-param . params)
                                   (error 'func "no overload matched"))])))]))
(define-syntax (define-instance stx)
    (syntax-parse stx
        [(_ func:id pred:expr proc:expr)
         #:with dispatcher-name
                (dispatcher-name #'func)
         #'(add-dispatch dispatcher-name pred proc)]))
(define-provide-syntax (interface-out stx)
    (syntax-parse stx
        [(_ func:id)
         #:with dispatcher-name
                (dispatcher-name #'func)
         #'(combine-out func dispatcher-name)]))
