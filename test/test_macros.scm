; define the macros

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define-syntax case
  (syntax-rules (else)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...)
                      body1 body2 ...)))
        tag)
      val ...))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...)
       body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...)
         body1 body2 ...)))))

(define-syntax begin
  (syntax-rules ()
    ((begin exp ...)
     ((lambda () exp ...)))))

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
         command ...)
     (letrec
       ((loop
         (lambda (var ...)
           (if test
               (begin
                 (if #f #f)
                 expr ...)
               (begin
                 command
                 ...
                 (loop (do "step" var step ...)
                       ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))

; run the tests

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
; TODO: case-2
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

(test 'do-1
      (do ((vec (make-vector 5))
	   (i 0 (+ i 1)))
	  ((= i 5) vec)
	(vector-set! vec i i))
      '#(0 1 2 3 4))
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
