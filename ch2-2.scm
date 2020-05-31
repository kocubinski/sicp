(define (println . args)
  (display args)
  (newline))

(define nil '())

;; Exercise 2.17
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(last-pair (list 28 72 149 34))
;Value: (34)

;; Exercise 2.18
(define (reverse x)
  (define (iter x y)
    (if (null? x)
	y
	(iter 
	 (cdr x)
	 (cons (car x) y))))
  (iter x nil))

(reverse (list 1 4 9 16 25))
