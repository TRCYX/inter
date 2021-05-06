;;; #lang racket

(define nil null)

;;; (define (map f xs)
;;;   (if (null? xs)
;;;       null
;;;       (cons (f (car xs))
;;;             (map f (cdr xs)))))

;;; (define (accumulate op initial sequence)
;;;   (if (null? sequence)
;;;       initial
;;;       (op (car sequence)
;;;           (accumulate op initial (cdr sequence)))))

(define accumulate foldr)

;;; (define (filter predicate sequence)
;;;   (cond ((null? sequence) nil)
;;;         ((predicate (car sequence))
;;;          (cons (car sequence)
;;;                (filter predicate (cdr sequence))))
;;;         (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (length items)
  (if (null? items) 0 (+ 1 (length (cdr items)))))

(define (queens board-size)
  (define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))
  (define empty-board nil)
  (define (safe? k positions)
    (define (check kth-position rest-positions delta-col)
      (or (null? rest-positions)
          (and (not (= kth-position (car rest-positions)))
               (not (= (+ kth-position delta-col) (car rest-positions)))
               (not (= (- kth-position delta-col) (car rest-positions)))
               (check kth-position (cdr rest-positions) (+ delta-col 1)))))
    (check (car positions) (cdr positions) 1))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position
                       new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 4)