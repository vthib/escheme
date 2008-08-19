(test 'procedure?-1 (procedure? car) #t)
(test 'procedure?-2 (procedure? 'car) #f)
(test 'procedure?-3 (procedure? (lambda (x) (* x x))) #t)
(test 'procedure?-4 (procedure? '(lambda (x) (* x x))) #f)

(test-error 'apply-1 (apply 'a '(a b)))
(test 'apply-2 (apply cons (list 'a 'b)) '(a . b))
(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))
(test 'apply-3 ((compose list cons) 'a 'b) '((a . b)))
(test 'apply-4 (apply list 'a 'b '(c d)) '(a b c d))

(test-error 'map-1 (map 'a '(a)))
(test-error 'map-2 (map cons 'a))
(test-error 'map-3 (map cons '(a b) '(c d e)))
(test 'map-4 (map cadr '((a b) (d e) (g h))) '(b e h))
(test 'map-5
      (map
       (let ((old '()))
         (lambda (x)
           (let ((res (cons x old)))
             (set! old x)
             res)))
       '(a b c d e))
      '((a) (b . a) (c . b) (d . c) (e . d)))
(test 'map-6 (map cons '(a b c) '(d e f)) '((a . d) (b . e) (c . f)))

(test-error 'for-each-1 (for-each 'a '(a)))
(test-error 'for-each-2 (for-each cons '(a)))
(test 'for-each-3
      (let ((c '()))
        (for-each (lambda (x) (set! c (cons x c))) '(a b c d))
        c)
      '(d c b a))
(test 'for-each-4
      (let* ((l1 (list 'a))
             (l2 (list 'a 'b))
             (l3 (cons 'a 'b))
             (before (list l1 l2 l3)))
        (for-each set-cdr! (list l1 l2 l3) '(b '() '(b)))
        (cons before (list l1 l2 l3)))
      '(((a) (a b) (a . b)) . ((a . b) (a) (a b))))
