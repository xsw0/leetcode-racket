#lang racket

(define/contract (daily-temperatures T)
  (-> (listof exact-integer?) (listof exact-integer?))
  (let ([rt ((lambda (T)
               (define (iterator l rl n)
                 (if (empty? l)
                     (cons rl n)
                     (iterator (cdr l) (cons (car l) rl) (+ n 1))))
               (iterator T '() 0))
             T)])
    (define (f result rt pair-day-tem current-day)
      (if (empty? rt)
          result
          (let ([ele (car rt)])
            (cond [(empty? pair-day-tem)
                   (f (cons 0 result)
                      (cdr rt)
                      (list (cons current-day ele))
                      (- current-day 1))]
                  [(> (cdar pair-day-tem) ele)
                   (f (cons (- (caar pair-day-tem) current-day) result)
                      (cdr rt)
                      (cons (cons current-day ele) pair-day-tem)
                      (- current-day 1))]
                  [else
                   (f result
                      rt
                      (cdr pair-day-tem)
                      current-day)]))))
    (f '() (car rt) '() (cdr rt))))

(daily-temperatures '(73 74 75 71 69 72 76 73))