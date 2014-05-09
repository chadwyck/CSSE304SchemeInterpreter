(define apply-env-ref 
	(lambda (env symbol)
		"code for original apply-env"
		"new apply-env is on a previous slide"))
(define deref
	cell-ref)
(define set-ref! 
	set-cell!)

;	2. Use pairs
(define (cell val) 
	(cons val 'its-a-cell?!-_))
(define cell-ref car)
(define set-cell! set-car!)
(define (cell? x)
	(and (pair? x)
		   (eqv? (cdr x) 'its-a-cell)))
