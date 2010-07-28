(define-record foo #f x y)
(let ((a (make-foo '() #f)))
  (test 'record_p-1 (foo? a) #t)
  (test 'record_p-2 (foo? (list '() #f)) #f)
  (test 'record_p-3 (foo? (cons '() #f)) #f)
  (test 'record_get-1 (foo:x a) '())
  (test 'record_get-2 (foo:y a) #f)
  (set-foo:x! a '(a))
  (set-foo:y! a #t)
  (test 'record_set-1 (foo:x a) '(a))
  (test 'record_set-2 (foo:y a) #t))
(define-record bar foo z)
(let ((b (make-bar #f 'a '())))
  (test 'record_p-4 (foo? b) #t)
  (test 'record_p-5 (bar? b) #t)
  (test 'record_get-3 (foo:x b) #f)
  (test 'record_get-4 (foo:y b) 'a)
  (test 'record_get-5 (bar:z b) '())
  (set-foo:x! b #f)
  (set-foo:y! b 'b)
  (set-bar:z! b 't)
  (test 'record_set-3 (foo:x b) #f)
  (test 'record_set-4 (foo:y b) 'b)
  (test 'record_set-5 (bar:z b) 't))
