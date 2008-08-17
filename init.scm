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
;?number
(define (zero? z) (= z 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (odd? n) (and (integer? n) (= 1 (modulo n 2))))
(define (even? n) (and (integer? n) (= 0 (remainder n 2))))
(define (abs n) (if (< n 0) (- n) n))

(define (max x . next)
  (letrec ((maxrec
            (lambda (x list)
              (if (null? list) x (maxrec
                                  (if (> x (car list)) x (car list))
                                  (cdr list))))))
    (maxrec x next)))

(define (min x . next)
  (letrec ((minrec
            (lambda (x list)
              (if (null? list) x (minrec
                                  (if (< x (car list)) x (car list))
                                  (cdr list))))))
    (minrec x next)))
;>

;; Ports primitives
;?port
(define (call-with-input-file string proc)
  (let* ((port (open-input-file string))
         (ret (proc port)))
    (begin (close-input-port port) ret)))
(define (call-with-output-file string proc)
  (let* ((port (open-output-file string))
         (ret (proc port)))
    (begin (close-output-port port) ret)))
;>

;?macro
(define-syntax define-macro
  (syntax-rules ()
    ((define-macro (name . args) body)
     (define name (lambda args (apply begin body))))
    ((define-macro name (lambda args body))
     (define name (lambda args (apply begin body))))))

;?string
(define (test name x y) (if (equal? x y)
                            (printf "~s: passed.~%" name)
                            (error name y " expected, got " x)))
;:
(define (test name x y) (if (not (equal? x y))
                            (error name y 'expected, 'got x))))
;>
