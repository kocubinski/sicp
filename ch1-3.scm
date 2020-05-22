;; Exercise 1.29
(define (even? n) (= 0 (remainder n 2)))
(define (cube x) (* x x x))
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (identity x) x)
(define (println . args)
  (display args)
  (newline))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 2 0.01)

(define (simpsons-rule f a b n)
  (let ((h (/ (- b a) n)))
    (define (term k) (* (cond ((= k 0) 1)
			      ((= k n) 1)
			      ((even? k) 2)
			      (else 4))
			(f (+ a (* k h)))))
    (* (/ h 3)
       (sum term 0 inc n))))

(simpsons-rule cube 0 1 100)
;; => 1/4

;; Exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result
			  (term a)))))
  (iter a 0))

(sum identity 1 inc 10)
(sum-iter identity 1 inc 10)

;; Exercise 1.31
;; a)
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(= (factorial 5)
   (* 5 4 3 2 1))

;; pi / 4
(product (lambda (n)
	   (* (/ (dec n) n)
	      (/ (inc n) n)))
	 3.
	 (lambda (n) (+ n 2))
	 100)

(/ 3.1415 4)

;; b)
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a)
	      (* result (term a)))))
  (iter a 1))

(= (factorial 5)
   (* 5 4 3 2 1)
   (product-iter identity 1 inc 5))

;; Exercise 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner result (term a)))))
  (iter a null-value))

(= (sum identity 1 inc 10)
   (accumulate + 0 identity 1 inc 10)
   (accumulate-iter + 0 identity 1 inc 10))

;; Exercise 1.33

(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (filtered-accumulate combiner pred null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (pred a)
		    (term a)
		    null-value)
		(filtered-accumulate combiner
				     pred
				     null-value
				     term
				     (next a)
				     next
				     b))))

(= (+ 1 2 3 5 7)
   (filtered-accumulate + prime? 0 identity 1 inc 10))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (coprime a b)
  (= (gcd a b) 1))

(coprime 14 15)
(coprime 14 21)

(define (coprime-product n)
  (filtered-accumulate *
		       (lambda (m) (coprime m n))
		       1
		       identity
		       1
		       inc
		       n))

(= (* 1 3 7 9)
   (coprime-product 10))

;; Exercise 1.34

(define (f g)
  (g 2))

;; (f f)
;; (f 2)
;; (2 2)
;; The intepreter tries to apply 2 to 2


