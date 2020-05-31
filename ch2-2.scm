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

;; Exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(cc 100 us-coins)
(cc 100 (list 25 50 5 10 1))

;; No order doesn't matter.  All combinations are explored regardless of order.

;; Exercise 2.20
(define (same-parity . xs)
  (define (iter x n)
    (if (null? x)
	x
	(if (= 0 (modulo n 2))
	    (cons (car x)
		  (iter (cdr x) (+ n 1)))
	    (iter (cdr x) (+ n 1)))))
  (iter xs 0))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)

;; Exercise 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))
(square-list (list 2 4 6 8))
;Value: (4 16 36 64)

(define (square-list items)
  (map (lambda (x) (* x x)) items))
(square-list (list 2 4 6 8))
;Value: (4 16 36 64)

;; Exercise 2.22
;; Order of cons requires that the list be reversed, in fact this is exactly my implementation 
;; for list reversal.
;;
;; In the second doesn't really make sense, sequentially nested pairs, with the inner-most pair being
;; (nil . answer) and subsequent pairs with the previous pair as the first element, and the second
;; elemnt as the answer.

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;; Exercise 2.23
(define (foreach f xs)
  (if (null? xs)
      #t
      (begin
	(f (car xs))
	(foreach f (cdr xs)))))

(foreach (lambda (x) (newline) (display x))
	 (list 57 321 88))

;; Exercise 2.24
(list 1 (list 2 (list 3 4)))
;Value: (1 (2 (3 4)))

