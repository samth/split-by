#lang racket
(provide split-by)
(define (split-by l f)
  (if (null? l)
      null
      (let loop ([groups null] [acc (list (car l))] [l (cdr l)] [val (f (car l))])
        (cond
          [(null? l) (reverse (cons acc groups))]
          [else
           (define c (car l))
           (define v (f c))
           (if (equal? val v)
               (loop groups (cons c acc) (cdr l) val)
               (loop (cons (reverse acc) groups) (list c) (cdr l) v))]))))
(module+ test
  (require rackunit)
  (check-equal? (split-by (range 1.0 5.1 .5) floor)
                '((1.0 1.5) (2.0 2.5) (3.0 3.5) (4.0 4.5) (5.0))))
