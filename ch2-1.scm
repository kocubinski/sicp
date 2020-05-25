;; Chapter 2.1 Exercises

(define (println . args)
  (display args)
  (newline))

;; Exercise 2.1

(define (make-rat n d)
  (let ((g (gcd n d))
	(sign (if  (< (* n d) 0)
		   -
		   +)))
    (cons (sign (abs (/ n g))) (abs (/ d g)))))

(make-rat (+ 6) (- 4))
(make-rat (- 6) (+ 4))
(make-rat 6 4)
(make-rat (- 6) (- 4))

;; Exercise 2.2

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (midpoint-segment segment)
  (make-point
   (/ (+ (x-point (end-segment segment))
	 (x-point (start-segment segment)))
      2)
   (/ (+ (y-point (end-segment segment))
	 (y-point (start-segment segment)))
      2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point
 (midpoint-segment
  (make-segment
   (make-point 2 2)
   (make-point 2 4))))

;; Exercise 2.3

(define (make-rect s p)
  (cons s p))

(define (point-distance p1 p2)
  (sqrt (+ (square (- (x-point p2)
		      (x-point p1)))
	   (square (- (y-point p2)
		      (y-point p1))))))

(define (rect-length r)
  (point-distance (start-segment (car r))
		  (end-segment (car r))))

(define (rect-width r)
  (point-distance (start-segment (car r))
		  (cdr r)))

(define my-rect
  (make-rect (make-segment (make-point 0 0)
			   (make-point 0 4))
	     (make-point 8 0)))

;; abstraction barrier 

(define (rect-perimeter rect)
  (* 2 (+ (rect-length rect)
	  (rect-width rect))))

(define (rect-area r)
  (* (rect-length r)
     (rect-width r)))

(rect-width my-rect)
(rect-length my-rect)
(rect-perimeter my-rect)
(rect-area my-rect)

;; Exercise 2.4

(define (cons* x y)
  (lambda (m) (m x y)))

(define (car* z)
  (z (lambda (p q) p)))

(define (cdr* z)
  (z (lambda (p q) q)))

(cdr* (cons* 1 2))

;; Exercise 2.5

(define (cons-i a b)
  (let ((product (* (expt 2 a) (expt 3 b))))
    (lambda (m)
      (round
       (cond
	((= m 0) (/ (log (/ product (expt 3 b)))
		    (log 2)))
	((= m 1) (/ (log (/ product (expt 2 a)))
		    (log 3))))))))

(define (car-i z) (z 0))
(define (cdr-i z) (z 1))

(car-i (cons-i 4 5))
(cdr-i (cons-i 4 5))

;; Exercise 2.6
;; /Church numerals/ - from Alonzo Church

;; Starting with zero not applying the function f at all
(define zero
  (lambda (f)
    (lambda (x) x)))

;; Proceed with 1 applying the function once

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

;; Applying the function twice
(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

;; Where n is an aribtrary church numeral
;; (succesor for Church numeral)
(define (add-1 n)
  (lambda (f)
    (lambda (x) (f ((n f) x)))))

;; For example, given the numerical primitive successor function
(define inc (lambda (x) (+ x 1)))

((two inc) 0)
;;Value: 2
(((add-1 two) inc) 0)
;;Value: 3

(define three
  (add-1 two))

((three inc) 0)

(define (add n m)
  (lambda (f)
    (lambda (x)
      ((n f) ((m f) x)))))

(((add two three) inc) 0)
;Value: 5
