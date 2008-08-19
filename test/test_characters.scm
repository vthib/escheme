(test 'char?-1 (if (char? #\A) 't 'f) 't)
(test 'char?-2 (char? 'a) #f)

(test-error 'char=?-1 (char=? 'a #\a))
(test-error 'char=?-2 (char=? #\a 'a))
(test 'char=?-3 (char=? #\a #\a) #t)
(test 'char=?-4 (char=? #\a #\A) #f)
(test 'char=?-5 (char=? #\a #\alarm) #f)

(test-error 'char>?-1 (char>? 'a #\a))
(test-error 'char>?-2 (char>? #\a 'a))
(test 'char>?-3 (char>? #\a #\b) #f)
(test 'char>?-4 (char>? #\b #\a) #t)
(test 'char>?-5 (char>? #\A #\a) #f)
(test 'char>?-6 (char>? #\0 #\9) #f)
(test 'char>?-7 (char>? #\0 #\a) #f)
(test 'char>?-8 (char>? #\0 #\A) #f)
(test 'char>?-9 (char>? #\a #\a) #f)

(test-error 'char<?-1 (char<? 'a #\a))
(test-error 'char<?-2 (char<? #\a 'a))
(test 'char<?-3 (char<? #\a #\b) #t)
(test 'char<?-4 (char<? #\b #\a) #f)
(test 'char<?-5 (char<? #\A #\a) #t)
(test 'char<?-6 (char<? #\0 #\9) #t)
(test 'char<?-7 (char<? #\0 #\a) #t)
(test 'char<?-8 (char<? #\0 #\A) #t)
(test 'char<?-9 (char<? #\a #\a) #f)

(test-error 'char>=?-1 (char>=? 'a #\a))
(test-error 'char>=?-2 (char>=? #\a 'a))
(test 'char>=?-3 (char>=? #\a #\b) #f)
(test 'char>=?-4 (char>=? #\b #\a) #t)
(test 'char>=?-5 (char>=? #\A #\a) #f)
(test 'char>=?-6 (char>=? #\0 #\9) #f)
(test 'char>=?-7 (char>=? #\0 #\a) #f)
(test 'char>=?-8 (char>=? #\0 #\A) #f)
(test 'char>=?-9 (char>=? #\a #\a) #t)

(test-error 'char<=?-1 (char<=? 'a #\a))
(test-error 'char<=?-2 (char<=? #\a 'a))
(test 'char<=?-3 (char<=? #\a #\b) #t)
(test 'char<=?-4 (char<=? #\b #\a) #f)
(test 'char<=?-5 (char<=? #\A #\a) #t)
(test 'char<=?-6 (char<=? #\0 #\9) #t)
(test 'char<=?-7 (char<=? #\0 #\a) #t)
(test 'char<=?-8 (char<=? #\0 #\A) #t)
(test 'char<=?-9 (char<=? #\a #\a) #t)

(test-error 'char-ci=?-1 (char-ci=? 'a #\a))
(test-error 'char-ci=?-2 (char-ci=? #\a 'a))
(test 'char-ci=?-3 (char-ci=? #\a #\a) #t)
(test 'char-ci=?-4 (char-ci=? #\a #\A) #t)
(test 'char-ci=?-5 (char-ci=? #\a #\alarm) #f)

(test-error 'char-ci>?-1 (char-ci>? 'a #\a))
(test-error 'char-ci>?-2 (char-ci>? #\a 'a))
(test 'char-ci>?-3 (char-ci>? #\a #\b) #f)
(test 'char-ci>?-4 (char-ci>? #\b #\a) #t)
(test 'char-ci>?-5 (char-ci>? #\a #\A) #f)
(test 'char-ci>?-6 (char-ci>? #\0 #\9) #f)
(test 'char-ci>?-7 (char-ci>? #\0 #\a) #f)
(test 'char-ci>?-8 (char-ci>? #\0 #\A) #f)
(test 'char-ci>?-9 (char-ci>? #\a #\a) #f)

(test-error 'char-ci<?-1 (char-ci<? 'a #\a))
(test-error 'char-ci<?-2 (char-ci<? #\a 'a))
(test 'char-ci<?-3 (char-ci<? #\a #\b) #t)
(test 'char-ci<?-4 (char-ci<? #\b #\a) #f)
(test 'char-ci<?-5 (char-ci<? #\A #\a) #f)
(test 'char-ci<?-6 (char-ci<? #\0 #\9) #t)
(test 'char-ci<?-7 (char-ci<? #\0 #\a) #t)
(test 'char-ci<?-8 (char-ci<? #\0 #\A) #t)
(test 'char-ci<?-9 (char-ci<? #\a #\a) #f)

(test-error 'char-ci>=?-1 (char-ci>=? 'a #\a))
(test-error 'char-ci>=?-2 (char-ci>=? #\a 'a))
(test 'char-ci>=?-3 (char-ci>=? #\a #\b) #f)
(test 'char-ci>=?-4 (char-ci>=? #\b #\a) #t)
(test 'char-ci>=?-5 (char-ci>=? #\a #\A) #t)
(test 'char-ci>=?-6 (char-ci>=? #\0 #\9) #f)
(test 'char-ci>=?-7 (char-ci>=? #\0 #\a) #f)
(test 'char-ci>=?-8 (char-ci>=? #\0 #\A) #f)
(test 'char-ci>=?-9 (char-ci>=? #\a #\a) #t)

(test-error 'char-ci<=?-1 (char-ci<=? 'a #\a))
(test-error 'char-ci<=?-2 (char-ci<=? #\a 'a))
(test 'char-ci<=?-3 (char-ci<=? #\a #\b) #t)
(test 'char-ci<=?-4 (char-ci<=? #\b #\a) #f)
(test 'char-ci<=?-5 (char-ci<=? #\A #\a) #t)
(test 'char-ci<=?-6 (char-ci<=? #\0 #\9) #t)
(test 'char-ci<=?-7 (char-ci<=? #\0 #\a) #t)
(test 'char-ci<=?-8 (char-ci<=? #\0 #\A) #t)
(test 'char-ci<=?-9 (char-ci<=? #\a #\a) #t)

(test-error 'char-upcase-1 (char-upcase 'a))
(test 'char-upcase-2 (char-upcase #\a) #\A)
(test 'char-upcase-3 (char-upcase #\F) #\F)
(test 'char-upcase-4 (char-upcase #\space) #\space)

(test-error 'char-downcase-1 (char-downcase 'a))
(test 'char-downcase-2 (char-downcase #\a) #\a)
(test 'char-downcase-3 (char-downcase #\F) #\f)
(test 'char-downcase-4 (char-downcase #\space) #\space)

;?number
(test-error 'integer->char-1 (integer->char #\a))
(test-error 'integer->char-2 (integer->char -5))
(test 'integer->char-3 (integer->char 0) #\x0)
(test 'integer->char-4 (integer->char 65) #\A)
(test 'integer->char-5 (integer->char 32) #\space)
(test 'integer->char-6 (integer->char 96) #\`)

(test-error 'char-alphabetic?-1 (char-alphabetic? 'a))
(do ((i 0 (+ i 1)))
    ((= i 128))
  (cond
   ((and (>= i 65) (<= i 90))
    (test 'char-alphabetic?-2 (char-alphabetic? (integer->char i)) #t))
   ((and (>= i 97) (<= i 122))
    (test 'char-alphabetic?-3 (char-alphabetic? (integer->char i)) #t))
   (else
    (test 'char-alphabetic?-4 (char-alphabetic? (integer->char i)) #f))))

(test-error 'char-numeric?-1 (char-numeric? 'a))
(do ((i 0 (+ i 1)))
    ((= i 128))
  (if (and (>= i 48) (<= i 57))
      (test 'char-numeric?-2 (char-numeric? (integer->char i)) #t)
      (test 'char-numeric?-3 (char-numeric? (integer->char i)) #f)))

(test-error 'char-whitespace?-1 (char-whitespace? 'a))
(do ((i 0 (+ i 1)))
    ((= i 128))
  (case i
    ((9 10 11 12 13 32) (test 'char-whitespace?-2 (char-whitespace? (integer->char i)) #t))
    (else (test 'char-whitespace?-3 (char-whitespace? (integer->char i)) #f))))

(test-error 'char-upper-case?-1 (char-upper-case? 'a))
(do ((i 0 (+ i 1)))
    ((= i 128))
  (if (and (>= i 65) (<= i 90))
      (test 'char-upper-case?-2 (char-upper-case? (integer->char i)) #t)
      (test 'char-upper-case?-3 (char-upper-case? (integer->char i)) #f)))

(test-error 'char-lower-case?-1 (char-lower-case? 'a))
(do ((i 0 (+ i 1)))
    ((= i 128))
  (if (and (>= i 97) (<= i 122))
      (test 'char-lower-case?-2 (char-lower-case? (integer->char i)) #t)
      (test 'char-lower-case?-3 (char-lower-case? (integer->char i)) #f)))

(test-error 'char->integer-1 (char->integer 0))
(test 'char->integer-2 (char->integer #\x0) 0)
(test 'char->integer-3 (char->integer #\A) 65)
(test 'char->integer-4 (char->integer #\space) 32)
(test 'char->integer-5 (char->integer #\`) 96)
;>
