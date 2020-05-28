;; -*- geiser-scheme-implementation: mit -*-

;; Chapter 2.1 Exercises

(define (square x) (* x x))
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

;; Exercise 2.7

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

;; Exercise 2.8
(define one->three (make-interval 1 3))
(add-interval one->three one->three)
(div-interval one->three one->three)

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
		 (- (upper-bound x) (upper-bound y))))

(sub-interval one->three one->three)

;; Exercise 2.9
(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x))
     2))

(define one->four (make-interval 1. 4))
(width-interval one->four)

(= (width-interval (add-interval one->four one->three))
   (+ (width-interval one->four)
      (width-interval one->three)))

(= (width-interval (sub-interval one->four one->three))
   (- (width-interval one->four)
      (width-interval one->three)))

(= (width-interval (mul-interval one->four one->three))
   (* (width-interval one->four)
      (width-interval one->three)))

;; Exercise 2.10
(define (div-interval x y)
  (if (= 0 (- (upper-bound y)
	      (lower-bound y)))
      (error "span of divisor must not be zero")
      (mul-interval x 
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))


;; Exercise 2.11
(define (mul-interval-2 x y)
  (define (enc n)
    (if (positive? n) "p" "n"))
  (let ((xl (lower-bound x))
	(xu (upper-bound x))
	(yl (lower-bound y))
	(yu (upper-bound y)))
    (let ((word (string-append (enc xl) (enc xu)
			       (enc yl) (enc yu))))
      (cond
       ((string=? word "pppp") (make-interval (* xl yl) (* xu yu)))
       ((string=? word "nppp") (make-interval (* xl yu) (* xu yu)))
       ((string=? word "nnpp") (make-interval (* xl yu) (* xu yl)))

       ((string=? word "ppnp") (make-interval (* xu yl) (* xu yu)))
       ((string=? word "npnp") (make-interval (min (* xl yu) (* xu yl))
					      (max (* xl yl) (* xu yu))))
       ((string=? word "nnnp") (make-interval (* xl yu) (* xu yl)))

       ((string=? word "ppnn") (make-interval (* xu yl) (* xl yu)))
       ((string=? word "npnn") (make-interval (* xu yl) (* xl yl)))
       ((string=? word "nnnn") (make-interval (* xu yu) (* xl yl)))))))

(mul-interval-2
 (make-interval 2 5)
 (make-interval 4 6))

;; TODO: tests.

;; Exercise 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (- c (* c p))
		 (+ c (* c p))))

(define (percent i)
  (- 1 (/ (lower-bound i)
	  (center i))))

(percent 
 (make-center-percent 3.5 0.05))

;; Exercise 2.13

(define interval-2.13-1 (make-center-percent 3.5 0.05))
(define interval-2.13-2 (make-center-percent 5 0.02))

(percent
 (mul-interval interval-2.13-1 interval-2.13-2))

(define (mul-percent x y)
  (+ (percent x) (percent y)))

(mul-percent interval-2.13-1 interval-2.13-2)
