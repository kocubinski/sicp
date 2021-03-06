(define (println . args)
  (display args)
  (newline))

(define (print x)
  (display x)
  (newline))

(define nil '())

(define (identity x) x)

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

;; Exercise 2.25
(car (cdaddr '(1 3 (5 7) 9)))

(car '(7))

(cadadr
 (cadadr
  (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))

;; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
;; (1 2 3 4 5 6)

(cons x y)
;; ((1 2 3) 4 5 6)

(list x y)
;; ((1 2 3) (4 5 6))

;; Exercise 2.27

(define reverse-me (list (list 1 2) (list 3 4)))

(define (deep-reverse xs)
  (reverse (map reverse xs)))

(deep-reverse reverse-me)

;;Exercise 2.28
(define x (list (list 1 2) (list 3 4)))

(define (filter-nil x)
  (print x)
  (cond
   ((null? x) x)
;; this is also sensible and more clear me
;; ((not (pair? x)) x)
   ((null? (car x)) (filter-nil (cdr x)))
   (else (cons (car x) (filter-nil (cdr x))))))

(define (ugly-fringe x)
  (define (flatten x)
    (if (not (pair? x))
	(list x)
	(append (flatten (car x))
		(flatten (cdr x)))))
  (filter-nil (flatten x)))

(filter-nil (list 1 2 3 '() 4 5 '()))

(define (fringe x)
  (print x)
  (cond ((null? x) nil)
	((not (pair? x)) (list x))
	(else (append (fringe (car x))
		      (fringe (cdr x))))))

(fringe x)

(fringe (list x x))

;; Exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define left-branch car)

(define right-branch cadr)

(define branch-length car)

(define branch-structure cadr)

(define (total-weight-1 x)
  (cond ((null? x) 0)
	((number? (car x))
	 (if (number? (cadr x))
	     (cadr x)
	     (+ (total-weight-1 (cdr x)))))
	(else (+ (total-weight-1 (car x))
		 (total-weight-1 (cdr x))))))

(define (total-weight x)
  (cond ((list? (car x))
	 (+ (total-weight (left-branch x))
	    (total-weight (right-branch x))))
	((list? (branch-structure x))
	 (total-weight (branch-structure x)))
	(else
	 (branch-structure x))))

(define mobile-a
  (make-mobile
   (make-branch 10
		(make-mobile
		 (make-branch 5 11)
		 (make-branch 4 13)))
   (make-branch 7 20)))

(= (total-weight-1 mobile-a)
   (total-weight mobile-a)
   (+ 11 13 20))

(define (torque x)
  (cond ((list? (car x))
	 (+ (torque (left-branch x))
	    (torque (right-branch x))))
	((list? (branch-structure x))
	 (* (branch-length x) (torque (branch-structure x))))
	(else
	 (* (branch-length x) (branch-structure x)))))

(= (torque mobile-a)
   (+ (* 10 (+ (* 5 11)
	       (* 4 13)))
      (* 7 20)))

(define (balanced? mobile)
  (= (torque (left-branch mobile))
     (torque (right-branch mobile))))

(balanced? mobile-a)
(balanced? (make-mobile mobile-a mobile-a))

;; part (d)

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define left-branch car)

(define right-branch cdr)

(define branch-length car)

(define branch-structure cdr)

(define (total-weight x)
  (cond ((pair? (car x))
	 (+ (total-weight (left-branch x))
	    (total-weight (right-branch x))))
	((pair? (branch-structure x))
	 (total-weight (branch-structure x)))
	(else
	 (branch-structure x))))

(define mobile-a
  (make-mobile
   (make-branch 10
		(make-mobile
		 (make-branch 5 11)
		 (make-branch 4 13)))
   (make-branch 7 20)))

(= (total-weight mobile-a)
   (+ 11 13 20))

;; modified selectors from cadr -> cdr
;; modified predicate from list? -> pair?

;; Exercise 2.30

(define (square-tree tree)
  (map (lambda (x)
	 (if (pair? x)
	     (square-tree x)
	     (* x x)))
       tree))

(define (square-tree-2 x)
  (cond ((null? x) nil)
        ((not (pair? x)) (* x x))
        (else (cons (square-tree-2 (car x))
                    (square-tree-2 (cdr x))))))

(define square-tree-input
  (list 1
	(list 2 (list 3 4) 5)
	(list 6 7)))

(square-tree square-tree-input)
;; (1 (4 (9 16) 25) (36 49))

(square-tree-2 square-tree-input)
;; (1 (4 (9 16) 25) (36 49))

;; Exercise 2.31

(define (tree-map f tree)
  (map (lambda (x)
	 (if (pair? x)
	     (tree-map f x)
	     (f x)))
       tree))

(define (square-tree tree) (tree-map square tree))
(square-tree square-tree-input)
;Value: (1 (4 (9 16) 25) (36 49))

;; Exercise 2.32

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(print rest)
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))

;; Exercise 2.33

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 4 4 4))

(map (lambda (n) (* 2 n)) (list 1 2 3 4 5))
;;Value: (2 4 6 8 10)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length (list 1 2 3 4 5 6))

;; Exercise 2.34
;; Horner's Rule

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;; For example, to compute 1 + 3x + 5x^3 + x^5 at x = 2 you would evaluate
;; (horner-eval 2 (list 1 3 0 5 0 1))

(= 79
   (let ((x 2))
     (+ 1
	(* 3 x)
	(* 5 (expt x 3))
	(* 1 (expt x 5))))
   (horner-eval 2 (list 1 3 0 5 0 1)))

;; Exercise 2.35

(define (count-leaves* x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves* (car x))
                 (count-leaves* (cdr x))))))

(define (count-leaves t)
  (accumulate (lambda (x acc)
		(+ acc
		   (if (pair? x)
		       (count-leaves x)
		       x)))
	      0
	      (map (lambda (x)
		     (if (pair? x)
			 x
			 1))
		   t)))

(= 
 (count-leaves* square-tree-input)
 (count-leaves square-tree-input))

;; Exercise 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

;; Exercise 2.37

(define matrix-a
  '((1 2 3 4)
    (4 5 6 6)
    (6 7 8 9)))

(define vector-a '(2 2 2 2))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product vector-a vector-a)

(define (matrix-*-vector m v)
  (map (lambda (r)
	 (accumulate + 0 (map * r v)))
       m))

(matrix-*-vector matrix-a vector-a)

(define (transpose mat)
  (accumulate-n cons nil mat))

(transpose matrix-a)
'((1 4 6)
  (2 5 7)
  (3 6 8)
  (4 6 9))

(define matrix-b
  '(( 0  4 -2)
    (-4 -3  0)))

(define matrix-c
  '((0  1)
    (1 -1)
    (2  3)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
	   (map (lambda (col)
		  (accumulate + 0
			      (map * row col)))
		cols))
	 m)))

(matrix-*-matrix matrix-b matrix-c)

;; Exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(fold-right / 1 (list 1 2 3))
(/ 
 (/ (/ 3 1)
    2)
 1)
;; 3/2
(fold-left / 1 (list 1 2 3))
(/ 
 (/ 1 2)
 3)
;; 1/6
(fold-right list nil (list 1 2 3))
;; (1 (2 (3 ())))
(fold-left list nil (list 1 2 3))
;; (((() 1) 2) 3)
;; op must satisfy the associative property

;; Exercise 2.39
(define reverse-test (list 1 2 3 4 5))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(reverse reverse-test)

(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))
(reverse reverse-test)

