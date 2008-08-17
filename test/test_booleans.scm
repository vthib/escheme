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
