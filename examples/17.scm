(define nil null)

(define (last-pair items)
  (let ((next (cdr items)))
    (if (null? next)
        (car items)
        (last-pair next))))

(define (reverse items)
  (define (iter result items)
    (if (null? items)
        result
        (iter (cons (car items)
                    result)
              (cdr items))))
  (iter nil items))

;(define one-through-four (list 1 2 3 4))
;(define odds (list 1 3 5 7))
;(define squares (list 1 4 9 16))

(define (cc amount coin-values)
  (define (first-denomination coin-values)
    (car coin-values))
  (define (except-first-denomination coin-values)
    (cdr coin-values))
  (define (no-more? coin-values)
    (null? coin-values))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                  coin-values))
            (cc (- amount
                   (first-denomination
                     coin-values))
                coin-values)))))

;(define us-coins (list 50 25 10 5 1))
;(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (same-parity first . y)
  (define (iter y yes?)
    ;;; (print y yes? (null? y))
    (if (null? y)
        nil
        (let ((cur (car y))
              (left (cdr y)))
          (if (yes? cur)
              (cons cur (iter left yes?))
              (iter left yes?)))))
  (cons first (iter y
                (λ (x)
                  ;;; (print x)
                  (= (% x 2)
                     (% first 2))))))

(print (same-parity 2 3 4 5 6 7))