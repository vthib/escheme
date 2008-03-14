; here are the twenty eigth compositions of car and cdr required by r5rs
(define (caar pair) (car (car pair)))
(define (cadr pair) (car (cdr pair)))
(define (cdar pair) (cdr (car pair)))
(define (cddr pair) (cdr (cdr pair)))
(define (caaar pair) (car (car (car pair))))
(define (caadr pair) (car (car (cdr pair))))
(define (cadar pair) (car (cdr (car pair))))
(define (caddr pair) (car (cdr (cdr pair))))
(define (cdaar pair) (cdr (car (car pair))))
(define (cdadr pair) (cdr (car (cdr pair))))
(define (cddar pair) (cdr (cdr (car pair))))
(define (cdddr pair) (cdr (cdr (cdr pair))))
(define (caaaar pair) (car (car (car (car pair)))))
(define (caaadr pair) (car (car (car (cdr pair)))))
(define (caadar pair) (car (car (cdr (car pair)))))
(define (caaddr pair) (car (car (cdr (cdr pair)))))
(define (cadaar pair) (car (cdr (car (car pair)))))
(define (cadadr pair) (car (cdr (car (cdr pair)))))
(define (caddar pair) (car (cdr (cdr (car pair)))))
(define (cadddr pair) (car (cdr (cdr (cdr pair)))))
(define (cdaaar pair) (cdr (car (car (car pair)))))
(define (cdaadr pair) (cdr (car (car (cdr pair)))))
(define (cdadar pair) (cdr (car (cdr (car pair)))))
(define (cdaddr pair) (cdr (car (cdr (cdr pair)))))
(define (cddaar pair) (cdr (cdr (car (car pair)))))
(define (cddadr pair) (cdr (cdr (car (cdr pair)))))
(define (cdddar pair) (cdr (cdr (cdr (car pair)))))
(define (cddddr pair) (cdr (cdr (cdr (cdr pair)))))

;; some numbers primitives can be defined as lambdas expressions
(define (zero? n) (= n 0))
(define (positive? n) (> n 0))
(define (negative? n) (< n 0))
(define (odd? n) (and (integer? n) (not (= 0 (modulo n 2)))))
(define (even? n) (and (integer? n) (= 0 (modulo n 2))))

(define (abs n) (if (< n 0) (- n) n))

(define (max n . next)
  (letrec ((maxrec
	 (lambda (n list)
	   (if (null? list) n (maxrec
			       (if (> n (car list)) n (car list))
			       (cdr list))))))
    (maxrec n next)))

(define (min n . next)
  (letrec ((minrec
	 (lambda (n list)
	   (if (null? list) n (minrec
			       (if (< n (car list)) n (car list))
			       (cdr list))))))
    (minrec n next)))

;; Ports primitives
(define (call-with-input-file string proc)
  (let* ((port (open-input-file string))
	 (ret (proc port)))
    (begin (close-input-port port) ret)))
(define (call-with-output-file string proc)
  (let* ((port (open-output-file string))
	 (ret (proc port)))
    (begin (close-output-port port) ret)))

; srfi 28
(define format
  (lambda (format-string . objects)
    (let ((buffer (open-output-string)))
      (let loop ((format-list (string->list format-string))
                 (objects objects))
        (cond ((null? format-list) (get-output-string buffer))
              ((char=? (car format-list) #\~)
               (if (null? (cdr format-list))
                   (error 'format "Incomplete escape sequence")
                   (case (cadr format-list)
                     ((#\a)
                      (if (null? objects)
                          (error 'format "No value for escape sequence")
                          (begin
                            (display (car objects) buffer)
                            (loop (cddr format-list) (cdr objects)))))
		     ((#\s)
                      (if (null? objects)
                          (error 'format "No value for escape sequence")
                          (begin
                            (write (car objects) buffer)
                            (loop (cddr format-list) (cdr objects)))))
                     ((#\%)
                      (newline buffer)
                      (loop (cddr format-list) objects))
                     ((#\~)
                      (write-char #\~ buffer)
                      (loop (cddr format-list) objects))
                     (else
                      (error 'format "Unrecognized escape sequence")))))
              (else (write-char (car format-list) buffer)
                    (loop (cdr format-list) objects)))))))

(define (printf format . args)
  (do ((i 0 (+ i 1))
       (len (string-length format)))
      ((>= i len))
    (if (char=? (string-ref format i) #\~)
	(if (= (+ i 1) len)
	    (error 'printf format " cannot end with a ~")
	    (begin (set! i (+ i 1))
		   (case (string-ref format i)
		     ((#\~) (write-char #\~))
		     ((#\a)
		      (if (null? args)
			  (error 'printf "Missing value for escape sequence.")
			  (begin
			    (display (car args))
			    (set! args (cdr args)))))
		     ((#\s)
		      (if (null? args)
			  (error 'printf "Missing value for escape sequence.")
			  (begin
			    (write (car args))
			    (set! args (cdr args)))))
		     ((#\% #\n) (write-char #\newline))
		     (else (error 'printf "unknown escape code ~"
				  (string-ref format i))))))
	(write-char (string-ref format i)))))

(define let-syntax let)
(define letrec-syntax letrec)

;; ALPHAS

(define-syntax alpha-set
  (syntax-rules ()
    ((alpha-set alpha name val) (with alpha (define name val)))))
(define-syntax alpha-get
  (syntax-rules ()
    ((alpha-get alpha name) (with alpha name))))
