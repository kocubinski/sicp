;; Chapter 1.2

;; Exercise 1.9
;; The first is recursive, the second is iterative.

;; Exercise 1.10 - Ackermann's Function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; (f n) -> 2(n+1)
;; (g n) -> 2n
;; (h n) ->

(define (inc n) (+ n 1))
(define (dec n) (- n 1))

;; Exercise 1.11

;; Fibonacci is f(n) = 

(define (println-depth depth . args)
  (display (make-string depth #\space))
  (display args)
  (newline))

(define (println . args)
  (display args)
  (newline))

(define (f-11-tree n)
  (define (f n depth)
    (println-depth depth n)
    (if (< n 3)
        n
        (+ (f (- n 1) (inc depth))
           (* 2 (f (- n 2) (inc depth)))
           (* 3 (f (- n 3) (inc depth))))))
  (f n 0))

;; a <- a + 2b + 3c
;; b <- a
;; c <- b

(define (f11-iter a b c n)
  (println a b c n)
  ;; degenerate cases
  (if (= 0 n)
      c
      (f11-iter (+ a (* 2 b) (* 3 c))
                a
                b
                (dec n))))

(define (f11 n)
  (f11-iter 2 1 0 n))
