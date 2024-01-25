#lang racket
(require racket/control)

(define nondet-prompt (new-prompt))

(define (pick lst)
  (shift0-at nondet-prompt k
    (for-each k lst)))

(define yield-prompt (new-prompt))

(define (yield v)
  (shift0-at yield-prompt k (cons v (k (void)))))

(define (stop)
  (shift0-at yield-prompt _k '(stopped)))

(define (pyth n)
  (reset0-at yield-prompt
    (reset0-at nondet-prompt
      (let* ([x (pick (range 1 n))]
             [y (pick (range x n))]
             [z (pick (range y n))])
        (when (equal? (+ (* x x) (* y y)) (* z z))
          (yield (list x y z))
          (when (equal? (+ (- z y) (- y x) (- z x)) n) (stop)) ; early termination
          )))
    '()))

;;;;

(define (pickm lst f)
  (append-map f lst))

(define (pyth2 n)
  (pickm (range 1 n) (lambda (x)
  (pickm (range x n) (lambda (y)
  (pickm (range y n) (lambda (z)
  (if (equal? (+ (* x x) (* y y)) (* z z))
    (list (list x y z))
    (list)))))))))

;;;;

(define (pickk lst f k)
  (match lst
    ['() (k (list))]
    [(cons x xs) (f x (lambda (y) (append y (pickk xs f k))))]))

(define (pyth3k n k)
  (pickk (range 1 n) (lambda (x k)
  (pickk (range x n) (lambda (y k)
  (pickk (range y n) (lambda (z k)
  (if (equal? (+ (* x x) (* y y)) (* z z))
    (k (list (list x y z)))
    (k (list)))) k)) k)) k))

(define (pyth3 n)
  (pyth3k n identity))
