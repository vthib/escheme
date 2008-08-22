(test 'vector-1 (vector) #())
(test 'vector-2 (vector 'a 'b 'c) #(a b c))
(test 'vector-3 (vector 'bar '(a b) (vector 'a 'foo) '())
	#(bar (a b) #(a foo) ()))

(test 'vector?-1 (vector? 'a) #f)
(test 'vector?-2 (vector? #('a)) #t)
(test 'vector?-3 (vector? #()) #t)

;?number
(test-error 'make-vector-1 (make-vector -5))
(test 'make-vector-2 (make-vector 0) #())
(test 'make-vector-3 (make-vector 5 'a) #(a a a a a))

(test 'vector-length-1 (vector-length #()) 0)
(test-error 'vector-length-2 (vector-length 5))
(test 'vector-length-3 (vector-length #(0 (2 2 2 2) #(Anna a))) 3)

(test-error 'vector-ref-1 (vector-ref 5 6))
(test-error 'vector-ref-2 (vector-ref #(a b c) -2))
(test-error 'vector-ref-3 (vector-ref #(a b c) 3))
(test-error 'vector-ref-4 (vector-ref #() 0))
(test 'vector-ref-5 (vector-ref #(a b c) 0) 'a)
(test 'vector-ref-6 (vector-ref #(0 (2 2 2 2) #(Anna a)) 2) #(Anna a))

(test-error 'vector-set!-1 (vector-set! 5 1 'a))
(test-error 'vector-set!-2 (vector-set! #(a b c) -2 'a))
(test-error 'vector-set!-3 (vector-set! #(a b c) 3 'a))
(test-error 'vector-set!-4 (vector-set! #() 0 2))
(test-error 'vector-set!-5 (vector-set! '#(a b c) 1 'a))
(test 'vector-set!-6
	(let ((v #(a (0 1 2) 5)))
		(vector-set! v 0 'b)
		v)
	#(b (0 1 2) 5))
(test 'vector-set!-7
	(let ((v #(a 5 (0 1 2))))
		(vector-set! v 2 'a)
		v)
	#(a 5 a))
;> number

(test-error 'vector->list-1 (vector->list 'a))
(test 'vector->list-2 (vector->list #()) '())
(test 'vector->list-3
      (vector->list #(dah #(dah ()) didah))
      '(dah #(dah ()) didah))

(test-error 'list->vector-1 (list->vector 'a))
(test 'list->vector-2 (list->vector '()) #())
(test 'list->vector-3 (list->vector
                       '(dah (dah ()) didah))
      #(dah (dah ()) didah))

(test-error 'vector-fill!-1 (vector-fill! 'a 'a))
(test-error 'vector-fill!-2 (vector-fill! '#(a a) 'b)) ; immutable vector
(test 'vector-fill!-3
	(let ((v #()))
		(vector-fill! #() 'a)
		v)
	#())
(test 'vector-fill!-4
	(let ((v #(a b c)))
		(vector-fill! v '(ab))
		v)
	#((ab) (ab) (ab)))
