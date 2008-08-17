(define (run-tests)
	(test 'promise-1
	  (let ((p (delay (+ 1 2))))
	    (list (force p) (force p)))
	  '(3 3))

	(define a-stream
	  (letrec ((next
	            (lambda (n)
	              (cons n (delay (next (+ n 1)))))))
	    (next 0)))
	(define head car)
	(define tail
	  (lambda (stream) (force (cdr stream))))
	(test 'promise-2 (head (tail (tail a-stream))) 2)

	(define count 0)
	(define p
	  (delay (begin (set! count (+ count 1))
	                (if (> count x)
	                    count
	                    (force p)))))
	(define x 5)
	(test 'promise-3 (list (force p) (begin (set! x 10) (force p))) '(6 6)))

(run-tests)

(define force
  (lambda (object)
    (object)))
(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (make-promise (lambda () expression)))))
(define make-promise
  (lambda (proc)
    (let ((result-ready? #f)
          (result #f))
      (lambda ()
        (if result-ready?
            result
            (let ((x (proc)))
              (if result-ready?
                  result
                  (begin (set! result-ready? #t)
                         (set! result x)
                         result))))))))
(run-tests)
