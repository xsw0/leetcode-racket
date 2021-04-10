#lang racket

; Definition for singly-linked list:


; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

(define/contract (add-two-numbers l1 l2)
  (-> (or/c list-node? #f) (or/c list-node? #f) (or/c list-node? #f))
  (define (iterator l1 l2 carry)
    (let* ([sum (+ (if l1 (list-node-val l1) 0)
                   (if l2 (list-node-val l2) 0)
                   carry)]
           [q (quotient sum 10)]
           [r (remainder sum 10)])
      (if (and (= sum 0) (not (or l1 l2)))
          #f
          (list-node r
                     (iterator (if l1 (list-node-next l1) #f)
                               (if l2 (list-node-next l2) #f)
                               q)))))
  (let ([l (iterator l1 l2 0)])
    (if l l (list-node 0 #f))))

(define (list->list-node l)
  (define (constructor l)
    (if (empty? l)
        #f
        (list-node (car l)
                   (constructor (cdr l)))))
  (if (empty? l)
      (list-node 0 #f)
      (constructor l)))

(add-two-numbers (list->list-node '(0))
                 (list->list-node '(0)))

(add-two-numbers (list->list-node '(9 9 9 9 9 9 9))
                 (list->list-node '(9 9 9 9)))