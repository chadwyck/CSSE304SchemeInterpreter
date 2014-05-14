; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (trace-lambda 31 ()
    (empty-env-record)))

(define extend-env
  (trace-lambda 32 (syms vals env)
    (extended-env-record syms vals env)))

(define extend-env-recursively
  (lambda (proc-names ids bodies old-env)
    (recursively-extended-env-record 
      proc-names ids bodies old-env)))

(define list-find-position
  (trace-lambda 33 (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record ()
        (fail))
      [extended-env-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
    	      (succeed (list-ref vals pos))
    	      (apply-env env sym succeed fail)))]
      [recursively-extended-env-record
        (procnames idss bodies old-env)
        (let ([pos 
              (list-find-position sym procnames)])
          (if (number? pos)
              (closure (list-ref idss pos)
                        (list (list-ref bodies pos))
                        env)
              (apply-env old-env sym succeed fail)))])))

