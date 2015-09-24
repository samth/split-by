#lang racket/base

(provide split-by)

(require racket/list)

(define (split-by l f)
  (if (null? l)
      null
      (let loop ([groups null] [acc (list (first l))] [l (rest l)] [val (f (first l))])
        (cond
          [(null? l) (reverse (cons acc groups))]
          [else
           (define c (first l))
           (define v (f c))
           (if (equal? val v)
               (loop groups (cons c acc) (rest l) val)
               (loop (cons (reverse acc) groups) (list c) (rest l) v))]))))

(module+ test
  (require rackunit)

  (define (should-never-be-called)
    (error "was called, shouldn't have been"))

  (check-equal? (split-by '() should-never-be-called)
                '())

  (check-equal? (split-by '(1.0 1.5 2.0 2.5 3.0) floor)
                '((1.0 1.5) (2.0 2.5) (3.0))))
