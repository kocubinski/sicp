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
  ;; degenerate case
  (if (= 0 n)
      c
      (f11-iter (+ a (* 2 b) (* 3 c))
                a
                b
                (dec n))))

(define (f11 n)
  (f11-iter 2 1 0 n))

;; Exercise 1.12
;;
;; (0) 1
;; (1) 1 1
;; (2) 1 2 1
;; (3) 1 3 3 1
;; (4) 1 4 6 4 1
;;

(define (pascal r c)
  (cond
   ((= c 0) 1)
   ((= c r) 1)
   (else
    (+ (pascal (dec r) c)
       (pascal (dec r) (dec c))))))

;; Exercise 1.13

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define phi (/ (+ 1 (sqrt 5))
	       2))

(define (fib-approx n)
  (round (/ (expt phi n)
	    (sqrt 5))))

(define (fib-test n)
  (= (fib n)
     (fib-approx n)))

;; Exercise 1.14

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (println amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; Exercise 1.15

(define (cube x) (* x x x))

(define (sine angle)
  (define (p x)
    (println x)
    (- (* 3 x) (* 4 (cube x))))
  (define (sine* angle)
    (if (not (> (abs angle) 0.1))
	angle
	(p (sine* (/ angle 3.0)))))
  (sine* angle))

(define logB 
  (lambda (x B) 
    (/ (log x) (log B))))

;; Exercise 1.16

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (println b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; 1 ]=> (fast-expt 2 9)
;; (2 9) = 2*256 = 512  [2^9]
;; (2 8) = 16^2  = 256  [2^8]
;; (2 4) = 4^2   = 16   [2^4]
;; (2 2) = 2^2   = 4    [2^2]
;; (2 1) = 2*1   = 2    [2^1]
;; (2 0)         = 1    [2^0]
;; ;Value: 512
;; 
;;
;; 1 ]=> (fast-expt 2 11)
;; (2 11) = 2048 = 1024*2
;; (2 10) = 1024 = 32^2 
;; (2 5)  = 32   = 16*2
;; (2 4)  = 16   = 4^2
;; (2 2)  = 4    = 2^2
;; (2 1)  = 2    = 1^2
;; (2 0)  = 1
;Value: 2048

(define (f16 b n)
  (define (f16-iter b n a)
    (println b n a (* a (expt b n)))
    (cond ((= n 0) a)
	  ((even? n) (f16-iter (square b) (/ n 2) a))
	  (else (f16-iter b (- n 1) (* b a)))))
  (f16-iter b n 1))

;; 1 ]=> (f16 2 11)
;; (2 11 1 2048)
;; (2 10 2 2048)
;; (4 5 2 2048)
;; (4 4 8 2048)
;; (16 2 8 2048)
;; (256 1 8 2048)
;; (256 0 2048 2048)
;Value: 2048

;; Exercise 1.17
(define (mult a b)
  (if (= b 0)
      0
      (+ a (mult a (- b 1)))))

(define (double n) (+ n n))
(define (halve n) (/ n 2))

(define (f17 a b)
  (println a b)
  (cond ((= b 0) 0)
	((even? b) (double (f17 a (halve b))))
	(else (+ a (f17 a (- b 1))))))

(define (f18 a b)
  (define (f18-iter (a b n))))
