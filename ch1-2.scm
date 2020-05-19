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
  ;; (println a b c n)
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

(define (fib-fast n)
  (define (fib-iter a b count)
    (if (= count 0)
	b
	(fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

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

;; Exercise 1.18

(define (f18 a b)
  (define (f18-iter a b n)
    (println a b n (* n a b))
    (cond ((= b 0) n)
	  ((even? b) (f18-iter (double a) (halve b) n))
	  (else (f18-iter a (- b 1) (+ a n)))))
  (f18-iter a b 0))

;; Exercise 1.19

;; a <- (+ (* b q) (* a q) (* a p))
;; b <- (+ (* b p) (* a q))

(define (f18-Tpq a b p q)
  (list (+ (* b q) (* a q) (* a p))
	(+ (* b p) (* a q))))

(define (f18-Tpq-prime a b p q)
  (let ((a-prime (first (f18-Tpq a b p q)))
	(b-prime (second (f18-Tpq a b p q))))
    (f18-Tpq a-prime b-prime p q)))

(define (f18-a-prime a b p q)
  (+ (* a (square p))
     (* 2 a p q)
     (* 2 a (square q))
     (* 2 b p q)
     (* b (square q))))

(define (f18-b-prime a b p q)
  (+ (* 2 a p q)
     (* a (square q))
     (* b (square p))
     (* b (square q))))

(define (f18-Tpq-prime-exp a b p q)
  (list (f18-a-prime a b p q) (f18-b-prime a b p q)))

(define (f18-Tpq-prime-i a b p q)
  (let ((ap (f18-a-prime a b p q))
	(bp (f18-b-prime a b p q)))
    (let ((qq (/ (- (* ap b) (* a bp))
		 (+ (square b) (* a b) (- (square a))))))
      (f18-Tpq a b
	       (if (= b 0)
		   1
		   (/ (- bp (* a qq))
		      b))
	       qq))))

(define (f18-Tpq-prime-p a b p q)
  (let ((ap (f18-a-prime a b p q))
	(bp (f18-b-prime a b p q)))
    (let ((pp (/ (- (* ap a) (* bp b) (* a bp))
		 (- (square a) (* a b) (square b)))))
      (f18-Tpq a b
	       pp
	       (/ (- bp (* b pp))
		  a)))))

(f18-Tpq 2 3 4 5) ;; => (33 22)
(f18-Tpq 33 22 4 5) ;; => (407 253)

(f18-Tpq-prime 2 3 4 5)
(f18-Tpq-prime-exp 2 3 4 5)
(f18-Tpq-prime-i 2 3 4 5)
(f18-Tpq-prime-p 2 3 4 5)

(f18-Tpq-prime 1 0 0 1)
(f18-Tpq-prime-exp 1 0 0 1)
(f18-Tpq-prime-i 1 0 0 1)
(f18-Tpq-prime-p 1 0 0 1)

(f18-Tpq 20 19 9 11) ;; => (609 391)
(f18-Tpq 609 391 9 11) ;; => (16481 10218)

(f18-Tpq-prime 20 19 9 11)
(f18-Tpq-prime-exp 20 19 9 11)
(f18-Tpq-prime-i 20 19 9 11)
(f18-Tpq-prime-p 20 19 9 11)

;; works, but suffers from divide by zero for odd n
(define (f18-fib-q n)
  (define (fib-iter a b p q count)
    (println a b p q count)
    (let ((ap (f18-a-prime a b p q))
	  (bp (f18-b-prime a b p q)))
      (let ((qq (/ (- (* ap b) (* a bp))
		   (+ (square b) (* a b) (- (square a))))))
	(cond ((= count 0) b)
	      ((even? count)
	       (fib-iter a
			 b
			 (/ (- bp (* a qq))
			    b)
			 qq		 
			 (/ count 2)))
	      (else (fib-iter (+ (* b q) (* a q) (* a p))
			      (+ (* b p) (* a q))
			      p
			      q
			      (- count 1)))))))
  (fib-iter 1 0 0 1 n))


(define (f18-fib-p n)
  (define (fib-iter a b p q count)
    (let ((ap (f18-a-prime a b p q))
	  (bp (f18-b-prime a b p q)))
      (let ((pp (/ (- (* ap a) (* bp b) (* a bp))
		   (- (square a) (* a b) (square b)))))
	(cond ((= count 0) b)
	      ((even? count)
	       (fib-iter a
			 b
			 pp
			 (/ (- bp (* b pp))
			    a)
			 (/ count 2)))
	      (else (fib-iter (+ (* b q) (* a q) (* a p))
			      (+ (* b p) (* a q))
			      p
			      q
			      (- count 1)))))))
  (fib-iter 1 0 0 1 n))

;; Exercise 1.20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

'((gcd 206 40)

  (gcd 40 (remainder 206 40))

  (gcd (remainder 206 40)
       (remainder 40 (remainder 206 40)))

  (gcd (remainder 40 (remainder 206 40))
       (remainder (remainder 206 40)
		  (remainder 40 (remainder 206 40))))

  (gcd (remainder (remainder 206 40)
		  (remainder 40 (remainder 206 40)))
       (remainder (remainder 40 (remainder 206 40))
		  (remainder (remainder 206 40)
			     (remainder 40 (remainder 206 40)))))

  (if (= 0 (remainder (remainder 40 (remainder 206 40))
		      (remainder (remainder 206 40)
				 (remainder 40 (remainder 206 40))))
	 (remainder (remainder 206 40)
		    (remainder 40 (remainder 206 40)))))
  )

;; normal-order results in 13
;; applicative-order results in 3

;; Primes

(define (fermat-demo)
  (define (t a n)
    (modulo (expt a n) n))
  (let ((prime 43)
	(not-prime 40))
    (list 
     (map (lambda (a) (list a (= a (t a prime))))
	  (iota (- prime 1) 1))
     (map (lambda (a) (list a (= a (t a not-prime))))
	  (iota (- not-prime 1) 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; Exercise 1.21

(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(smallest-divisor 199) ; => 199
(smallest-divisor 1999) ; => 1999
(smallest-divisor 19999) ; => 7

;; Exercise 1.22

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (begin (report-prime (- (runtime) start-time))
	     #t)
      #f))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes low high)
  (define (iter n found)
    (if (< found 3)
	(let ((p (timed-prime-test n)))
	  (iter (+ n 2) (if p (+ found 1) found)))))
  (iter (if (even? low) (+ 1 low) low) 0))

;; (search-for-primes 10000000000000 100000000000000)

;; Exercise 1.23

;; Redef above fn for this exercise
(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (next n)
    (if (= n 2)
	3
	(+ n 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))

;; 2x as fast now
;; (search-for-primes 10000000000000 100000000000000)

;; Exercise 1.24

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; Redeffing above for exercise

(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
      (begin (report-prime (- (runtime) start-time))
	     #t)
      #f))

;; (search-for-primes 10000000000000 100000000000000)

;; Exercise 1.25

(define (expmod-2 base exp m)
  (remainder (fast-expt base exp) m))

;; Technically yes, but performance is much worse.
;;
;; expmod has the advantage dealing with much smaller numbers, relying on
;; for any integers x, y, and m, we can find the remainder of x times y
;; modulo m by computing separately the remainders of x modulo m and y
;; modulo m, multiplying these, and then taking the remainder of the
;; result modulo m.
;;
;; expmod-2 computers a giant number and attempts division which is much
;; more expensive.

;; Exercise 1.26
;; Rewriting the expression in this way makes the algorith tree-recursive
;; for even numbers, exploding the number of steps.

;; Excercise 1.27

(fast-prime? 6601 9)
;; => #t

(define (thorough-fast-prime n)
  (define (iter m)
    (cond ((= m 1) #t)
	  ((= m (expmod m n n))
	   (iter (- m 1)))
	  (else #f)))
  (iter (- n 1)))

(thorough-fast-prime 6601)
;; passes for all m<n

;; Exercise 1.28
;; Miller-Rabin test

;; e.g.
(= (modulo (expt 3 42) 43)
   (modulo (expt 7 42) 43)
   (modulo (expt 18 42) 43)
   1)
