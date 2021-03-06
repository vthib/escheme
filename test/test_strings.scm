;?character
(test 'string-1 (string) "")
(test-error 'string-2 (string 'a))
(test 'string-3 (string #\b #\newline #\c #\") "b\nc\"")
(test-error 'string-4 (string #\b 'a #\c #\"))
(test-error 'string-5 (string 'a #\newline #\c #\"))
(test-error 'string-6 (string #\b #\newline #\c 'a))

(test 'string?-1 (string? "aha") #t)
(test 'string?-2 (string? '(#\a #\h #\a)) #f)

(test-error 'string-fill!-1 (string-fill! 'a #\a))
(test-error 'string-fill!-2 (string-fill! (string #\a #\b #\c) 'a))
(test-error 'string-fill!-3 (string-fill! "abc" #\a)) ; immutable string
(test 'string-fill!-4
      (let ((s (string)))
	(string-fill! s #\a)
	s)
      "")
(test 'string-fill!-5
      (let ((s (string #\a #\b #\c)))
	(string-fill! s #\y)
	s)
      "yyy")

(test-error 'string->list-1 (string->list 'a))
(test 'string->list-2 (string->list "") '())
(test 'string->list-3 (string->list "a\nd\"f") '(#\a #\newline #\d #\" #\f))

(test-error 'list->string-1 (list->string 'a))
(test 'list->string-2 (list->string '()) "")
(test 'list->string-3 (list->string '(#\a #\newline #\d #\" #\f)) "a\nd\"f")
;> character

;?number
(test-error 'make-string-1 (make-string -5))
(test 'make-string-2 (make-string 0) "")
(test 'make-string-3 (make-string 5 #\a) "aaaaa")
(test-error 'make-string-4 (make-string 5 8))

(test 'string-length-1 (string-length "") 0)
(test-error 'string-length-2 (string-length 5))
(test 'string-length-3 (string-length "ab\"\\az") 6)

(test-error 'substring-1 (substring 0 1 2))
(test-error 'substring-2 (substring "aha" #t 2))
(test-error 'substring-3 (substring "aha" 1 #t))
(test 'substring-4 (substring "aha" 1 1) "")
(test 'substring-5 (substring "" 0 0) "")
(test-error 'substring-6 (substring "abc" -1 0))
(test-error 'substring-7 (substring "abc" 0 4))
(test 'substring-8 (substring "abc" 0 3) "abc")
(test 'substring-9 (substring "abcdef" 1 4) "bcd")

;?character
(test-error 'string-ref-1 (string-ref 5 6))
(test-error 'string-ref-2 (string-ref "aha" -2))
(test-error 'string-ref-3 (string-ref "aha" 3))
(test-error 'string-ref-4 (string-ref "" 0))
(test 'string-ref-5 (string-ref "aha" 0) #\a)
(test 'string-ref-6 (string-ref "aa\n" 2) #\newline)

(test-error 'string-set!-1 (string-set! 5 1 #\a))
(test-error 'string-set!-2 (string-set! "aha" -2 #\a))
(test-error 'string-set!-3 (string-set! "aha" 3 #\a))
(test-error 'string-set!-4 (string-set! "aha" 1 2))
(test 'string-set!-5
      (let ((s (string #\h #\a #\a)))
	(string-set! s 0 #\a)
	s)
      "aaa")
(test 'string-set!-6
      (let ((s (string #\a #\a #\a)))
	(string-set! s 2 #\space)
	s)
      "aa ")
;> character
;> number

(test-error 'string=?-1 (string=? 'a "a"))
(test-error 'string=?-2 (string=? "a" 'a))
(test 'string=?-3 (string=? "" "") #t)
(test 'string=?-4 (string=? "a\nbv\"i" "a
bv\"i") #t)
(test 'string=?-5 (string=? "aBvi" "abvi") #f)

(test-error 'string>?-1 (string>? 'a "a"))
(test-error 'string>?-2 (string>? "a" 'a))
(test 'string>?-3 (string>? "" "") #f)
(test 'string>?-4 (string>? "haa" "gaa") #t)
(test 'string>?-5 (string>? "aag" "aah") #f)
(test 'string>?-6 (string>? "aag" "aag") #f)
(test 'string>?-7 (string>? "Aag" "aag") #f)
(test 'string>?-8 (string>? "aag" "aaG") #t)

(test-error 'string<?-1 (string<? 'a "a"))
(test-error 'string<?-2 (string<? "a" 'a))
(test 'string<?-3 (string<? "" "") #f)
(test 'string<?-4 (string<? "haa" "gaa") #f)
(test 'string<?-5 (string<? "aag" "aah") #t)
(test 'string<?-6 (string<? "aag" "aag") #f)
(test 'string<?-7 (string<? "Aag" "aag") #t)
(test 'string<?-8 (string<? "aag" "aaG") #f)

(test-error 'string>=?-1 (string>=? 'a "a"))
(test-error 'string>=?-2 (string>=? "a" 'a))
(test 'string>=?-3 (string>=? "" "") #t)
(test 'string>=?-4 (string>=? "haa" "gaa") #t)
(test 'string>=?-5 (string>=? "aag" "aah") #f)
(test 'string>=?-6 (string>=? "aag" "aag") #t)
(test 'string>=?-7 (string>=? "Aag" "aag") #f)
(test 'string>=?-8 (string>=? "aag" "aaG") #t)

(test-error 'string<=?-1 (string<=? 'a "a"))
(test-error 'string<=?-2 (string<=? "a" 'a))
(test 'string<=?-3 (string<=? "" "") #t)
(test 'string<=?-4 (string<=? "haa" "gaa") #f)
(test 'string<=?-5 (string<=? "aag" "aah") #t)
(test 'string<=?-6 (string<=? "aag" "aag") #t)
(test 'string<=?-7 (string<=? "Aag" "aag") #t)
(test 'string<=?-8 (string<=? "aag" "aaG") #f)

(test-error 'string-ci=?-1 (string-ci=? 'a "a"))
(test-error 'string-ci=?-2 (string-ci=? "a" 'a))
(test 'string-ci=?-3 (string-ci=? "" "") #t)
(test 'string-ci=?-4 (string-ci=? "a\nbv\"i" "a
bv\"i") #t)
(test 'string-ci=?-5 (string-ci=? "aBvi" "abvi") #t)

(test-error 'string-ci>?-1 (string-ci>? 'a "a"))
(test-error 'string-ci>?-2 (string-ci>? "a" 'a))
(test 'string-ci>?-3 (string-ci>? "" "") #f)
(test 'string-ci>?-4 (string-ci>? "haa" "gaa") #t)
(test 'string-ci>?-5 (string-ci>? "aag" "aah") #f)
(test 'string-ci>?-6 (string-ci>? "aag" "aag") #f)
(test 'string-ci>?-7 (string-ci>? "Aag" "aag") #f)
(test 'string-ci>?-8 (string-ci>? "aag" "aaG") #f)

(test-error 'string-ci<?-1 (string-ci<? 'a "a"))
(test-error 'string-ci<?-2 (string-ci<? "a" 'a))
(test 'string-ci<?-3 (string-ci<? "" "") #f)
(test 'string-ci<?-4 (string-ci<? "haa" "gaa") #f)
(test 'string-ci<?-5 (string-ci<? "aag" "aah") #t)
(test 'string-ci<?-6 (string-ci<? "aag" "aag") #f)
(test 'string-ci<?-7 (string-ci<? "Aag" "aag") #f)
(test 'string-ci<?-8 (string-ci<? "aag" "aaG") #f)

(test-error 'string-ci>=?-1 (string-ci>=? 'a "a"))
(test-error 'string-ci>=?-2 (string-ci>=? "a" 'a))
(test 'string-ci>=?-3 (string-ci>=? "" "") #t)
(test 'string-ci>=?-4 (string-ci>=? "haa" "gaa") #t)
(test 'string-ci>=?-5 (string-ci>=? "aag" "aah") #f)
(test 'string-ci>=?-6 (string-ci>=? "aag" "aag") #t)
(test 'string-ci>=?-7 (string-ci>=? "Aag" "aag") #t)
(test 'string-ci>=?-8 (string-ci>=? "aag" "aaG") #t)

(test-error 'string-ci<=?-1 (string-ci<=? 'a "a"))
(test-error 'string-ci<=?-2 (string-ci<=? "a" 'a))
(test 'string-ci<=?-3 (string-ci<=? "" "") #t)
(test 'string-ci<=?-4 (string-ci<=? "haa" "gaa") #f)
(test 'string-ci<=?-5 (string-ci<=? "aag" "aah") #t)
(test 'string-ci<=?-6 (string-ci<=? "aag" "aag") #t)
(test 'string-ci<=?-7 (string-ci<=? "Aag" "aag") #t)
(test 'string-ci<=?-8 (string-ci<=? "aag" "aaG") #t)

(test-error 'string-copy-1 (string-copy 'a))
(test 'string-copy-2 (string-copy "") "")
(test 'string-copy-3 (string-copy "abcdef") "abcdef")
; check if the copy is newly-allocated
;?number
;?character
(test 'string-copy-4
      (let* ((s (string #\a #\b #\c))
	     (copy (string-copy s)))
	(string-set! copy 1 #\d)
	(cons s copy))
      '("abc" . "adc"))
;>
;>
