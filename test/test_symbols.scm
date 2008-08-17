(test 'symbol?-1 (symbol? 'foo) #t)
(test 'symbol?-2 (symbol? (car '(a b))) #t)
(test 'symbol?-3 (symbol? 'nil) #t)
(test 'symbol?-4 (symbol? '()) #f)

(test 'symbol?-5 (symbol? (symbol->string 'a)) #f)
(test 'symbol?-6 (symbol? (string->symbol (symbol->string 'a))) #t)

(test 'symbol->string-1 (symbol->string 'flying-fish) "flying-fish")
(test 'symbol->string-2 (symbol->string 'MaRtiN) "MaRtiN")
(test 'symbol->string-3
  (symbol->string (string->symbol "Mal\nina"))
  "Mal\nina")
(set-case-sensitive! #f)
(test 'symbol->string-4 (symbol->string 'MaRtiN) "martin")
(test 'symbol->string-5
  (symbol->string (string->symbol "Mal\nina"))
  "Mal\nina")
(test-error 'symbol->string-6 (symbol->string "abc"))

(set-case-sensitive! #t)
(test 'string->symbol-1 (string->symbol "mISSISSIppi") 'mISSISSIppi)
(test 'string->symbol-2
  (symbol->string (string->symbol "Mal\nina"))
  "Mal\nina")
(set-case-sensitive! #f)
(test 'string->symbol-3
  (equal? (string->symbol "mISSISSIppi") 'mISSISSIppi)
  #f)
(test 'string->symbol-4
  (symbol->string (string->symbol "Mal\nina"))
  "Mal\nina")
(test-error 'string->symbol-5 (string->symbol 'abc))