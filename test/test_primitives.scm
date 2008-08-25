;?number

(test 'cond-1
      (cond ((> 3 2) 'greater)
	    ((< 3 2) 'less))
      'greater)
(test 'cond-2
      (cond ((> 3 3) 'greater)
      ((< 3 3) 'less)
      (else 'equal))
      'equal)
(test 'cond-3
      (cond ((assv 'b '((a 1) (b 2))) => cadr)
      (else #f))
      2)

(test 'case-1
      (case (* 2 3)
	((2 3 5 7) 'prime)
	((1 4 6 8 9) 'composite))
      'composite)
(test 'case-3
      (case (car '(c d))
	((a e i o u) 'vowel)
	((w y) 'semivowel)
	(else 'consonant))
      'consonant)

(test 'and-1 (and (= 2 2) (> 2 1)) #t)
(test 'and-2 (and (= 2 2) (< 2 1)) #f)
(test 'and-3 (and 1 2 'c '(f g)) '(f g))
(test 'and-4 (and) #t)

(test 'or-1 (or (= 2 2) (> 2 1)) #t)
(test 'or-2 (or (= 2 2) (< 2 1)) #t)
(test 'or-3 (or #f #f #f) #f)
(test 'or-4 (or (memq 'b '(a b c))  (/ 3 0)) '(b c))

(test 'let-1
      (let ((x 2) (y 3))
	(* x y))
      6)
(test 'let-2
      (let ((x 2) (y 3))
	(let ((x 7)
	      (z (+ x y)))
	  (* z x)))
      35)
(test 'let*
      (let ((x 2) (y 3))
	(let* ((x 7)
	       (z (+ x y)))
	  (* z x)))
      70)

;?vector
(test 'do-1
      (do ((vec (make-vector 5))
	   (i 0 (+ i 1)))
	  ((= i 5) vec)
	(vector-set! vec i i))
      '#(0 1 2 3 4))
;> vector
(test 'do-2
      (let ((x '(1 3 5 7 9)))
	(do ((x x (cdr x))
	     (sum 0 (+ sum (car x))))
	    ((null? x) sum)))
      25)
(test 'named-let
      (let loop ((numbers '(3 -2 1 6 -5))
		 (nonneg '())
		 (neg '()))
	(cond ((null? numbers) (list nonneg neg))
	      ((>= (car numbers) 0)
	       (loop (cdr numbers)
		     (cons (car numbers) nonneg)
		     neg))
	      ((< (car numbers) 0)
	       (loop (cdr numbers)
		     nonneg
		     (cons (car numbers) neg)))))
      '((6 1 3) (-5 -2)))

;> number

(if (lookup test-macros)
	(exit))

(test 'quasiquote-1
	`(list ,(+ 1 2) 4)
	'(list 3 4))
(test 'quasiquote-2
	(let ((name 'a)) `(list ,name ',name))
    '(list a (quote a)))
(test 'quasiquote-3
	`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
    '(a 3 4 5 6 b))
(test 'quasiquote-4
	`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
    '((foo 7) . cons))
(if (lookup sqrt)
	(test 'quasiquote-5
		`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)
		'#(10 5 2 4 3 8)))
(test 'quasiquote-6
	`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
	'(a `(b ,(+ 1 2) ,(foo 4 d) e) f))
(test 'quasiquote-7
	(let ((name1 'x)
		(name2 'y))
	`(a `(b ,,name1 ,',name2 d) e))
	'(a `(b ,x ,'y d) e))
