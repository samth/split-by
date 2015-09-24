#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [split-by (-> list? (-> any/c any/c) (listof list?))]))

(require racket/list)

(module+ test
  (require rackunit
           racket/function))


(define (split-by l f)
  (if (null? l)
      null
      (let*-values ([(split-val) (f (first l))]
                    [(first-split rest-vs)
                     (splitf-at l (lambda (v) (equal? (f v) split-val)))])
        (cons first-split (split-by rest-vs f)))))

(module+ test

  (check-equal? (split-by '() identity)
                '())

  (check-equal? (split-by '(1.0 1.5 2.0 2.5 3.0) floor)
                '((1.0 1.5) (2.0 2.5) (3.0))))
