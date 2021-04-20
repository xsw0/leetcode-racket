#lang racket

; Definition for singly-linked list:

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

(define/contract (rotate-right head k)
  (-> (or/c list-node? #f) exact-integer? (or/c list-node? #f))

  (define (length-tail l)
    (define (iterator l len)
      (let ([next (list-node-next l)])
        (if next
            (iterator next (+ len 1))
            (cons (+ len 1) l))))
    (iterator l 0))

  (define (find-kth head k)
    (if (= k 1)
        head
        (find-kth (list-node-next head) (- k 1))))
  
  (if (or (> k 0) (list-node? head))
      (let* ([p (length-tail head)]
             [len (car p)]
             [tail (cdr p)]
             [new-k (remainder k len)])
        (if (= 0 new-k)
            head
            (let* ([new-tail (find-kth head new-k)]
                   [new-head-temp (list-node-next new-tail)]
                   [new-head (list-node (list-node-val new-head-temp)
                                        (list-node-next new-head-temp))])
              (set-list-node-next! tail head)
              (set-list-node-next! new-tail #f)
              new-head)))
      head))

(define (list->list-node l)
  (define (constructor l)
    (if (empty? l)
        #f
        (list-node (car l)
                   (constructor (cdr l)))))
  (if (empty? l)
      (list-node 0 #f)
      (constructor l)))

(rotate-right (list->list-node '(1 2 3 4 5)) 2)
