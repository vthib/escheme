(test 'bool-1 '#t #t)
(test 'bool-2 '#f #f)

(test 'not-1 (not #t) #f)
(test 'not-2 (not 'a) #f)
(test 'not-3 (not '(a)) #f)
(test 'not-4 (not #f) #t)
(test 'not-5 (not '()) #f)

(test 'boolean?-1 (boolean? #f) #t)
(test 'boolean?-2 (boolean? #t) #t)
(test 'boolean?-3 (boolean? 'a) #f)

(test 'boolean-equal-1 (equal? #t #f) #f)
(test 'boolean-equal-2 (equal? #f #f) #t)
(test 'boolean-equal-3 (equal? #t #t) #t)

(test 'boolean-eqv-1 (eqv? #t #f) #f)
(test 'boolean-eqv-2 (eqv? #f #f) #t)
(test 'boolean-eqv-3 (eqv? #t #t) #t)

(test 'boolean-eq-1 (eq? #t #f) #f)
(test 'boolean-eq-2 (eq? #f #f) #t)
(test 'boolean-eq-3 (eq? #t #t) #t)

