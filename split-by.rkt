#lang racket/base

(provide split-by)

(require racket/list)

(module+ test
  (require rackunit
           racket/function))


(define (split-by l f)
  (if (null? l)
      null
      (let-values ([(first-split rest-vs) (split-by-once l f (f (first l)))])
        (cons first-split (split-by rest-vs f)))))

(module+ test

  (check-equal? (split-by '() identity)
                '())

  (check-equal? (split-by '(1.0 1.5 2.0 2.5 3.0) floor)
                '((1.0 1.5) (2.0 2.5) (3.0))))


(define (split-by-once vs f split-val)
  (define (should-include-in-split? v)
    (equal? (f v) split-val))
  (splitf-at vs should-include-in-split?))

(module+ test
  (define-values (split rest-vs)
    (split-by-once '(1.0 1.5 2.0 2.5 3.0) floor 1.0))
  (check-equal? split '(1.0 1.5))
  (check-equal? rest-vs '(2.0 2.5 3.0)))