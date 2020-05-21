;; Exercise 1.29
(define (even? n) (= 0 (remainder n 2)))
(define (cube x) (* x x x))
(define (inc n) (+ n 1))
(define (identity x) x)

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
