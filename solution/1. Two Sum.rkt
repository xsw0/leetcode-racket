#lang racket

(define/contract (two-sum nums target)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (define table (make-hash))
  (define (iterator nums index)
    (define distance (- target (car nums)))
    (if (hash-has-key? table distance)
        (list (hash-ref table distance)
              index)
        (begin (hash-set! table (car nums) index)
               (iterator (cdr nums) (+ index 1)))))
  (iterator nums 0))

(two-sum '(2 7 11 15) 9)
