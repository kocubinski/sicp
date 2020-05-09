(define pi 3.14159)

(+ (* 3
      (+ (* 2 4)
	 (+ 3 5)))
   (+ (- 10 7)
      6))

(define (square x) (* x x))

;; Ex 1.3

(define (ex-1.3 x y z)
  (max
   (+ (* x x) (* y y))
   (+ (* x x) (* z z))
   (+ (* y y) (* z z))))

;; if returns an operator
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; Ex 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; Then he evaluates the expression
;; (test 0 (p))
;; applicative eval will spin indefinetely. normal-order will early out, returning 0.

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (display (string-append (number->string guess)))
  (newline)
  (sleep-current-thread 50)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Ex 1.6 -
;; Using `new-if` in instead of the special form never exits. if, being a special form,
;; does not evaluate the else branch if predicate is true.
;; `new-if` (using applicative-order of the interpreter) always evalues the entire tree
;; (value bubbling upwards), which is infinte, and never reaches the bottom.

;; Ex 1.7
;; For very small numbers, (e.g. 0.0001) the guess will, while still quite far from the
;; root, become "good enough", falling within the specified range.
;;
;; For a very large number (e.g. 1e50) the algorithm will reach maximum recursion depth.
;; We cannot adequately represent 1e50 except by 16 digits of precision.
;; (square 1e25) => 1.0000000000000003e50 ; thus our good-enough? will never be within
;; bounds, even for precisely the right guess!

(define (sqrt-iter-v2 last-guess guess x)
  (display (string-append (number->string guess)))
  (newline)
  (sleep-current-thread 50)
  (if (< (abs (- last-guess guess)) 0.001)
      guess
      (sqrt-iter-v2 guess (improve guess x)
                 x)))

(define (sqrt-v2 x)
  (sqrt-iter-v2 100.0 1.0 x))

;; Ex 1.8

(define (improve-cube-guess guess x)
  (/ (+ (/ x (square guess))
	(* 2 guess))
     3))

(define (cuberoot-iter last-guess guess x)
  (display (string-append (number->string guess)))
  (newline)
  (sleep-current-thread 5)
  (if (< (abs (- last-guess guess)) 0.001)
      guess
      (cuberoot-iter guess (improve-cube-guess guess x)
                 x)))

(define (cuberoot x)
  (cuberoot-iter 100.0 1.0 x))
