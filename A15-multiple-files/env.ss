; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define reset-global-env 
  (lambda ()
    (set! global-env init-env)))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-by-one-env
  (lambda (syms vals env)
    (list 'extended-env-record (cons syms (cadr env)) (cons (box vals) (caddr env)))))

(define extend-env
  (lambda (syms vals env)
    (if (not ((list-of scheme-value?) vals))
      (extended-env-record syms (list (map box vals)) env)) 
      (extended-env-record syms (map box vals) env)))

(define extend-env-recursively
  (lambda (proc-names ids bodies old-env)
    (recursively-extended-env-record 
      proc-names ids bodies old-env)))

(define list-find-position
  (lambda (sym los)
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

;(define apply-env
;  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
;    (cases environment env
;      (empty-env-record ()
;        (fail))
;      [extended-env-record (syms vals env)
;        (let ((pos (list-find-position sym syms)))
;      	  (if (number? pos)
;    	      (if (list? vals)
;              (succeed (list-ref vals pos))
;              (succeed vals))
;    	      (apply-env env sym succeed fail)))]
;      [recursively-extended-env-record
;        (procnames idss bodies old-env)
;        (let ([pos 
;              (list-find-position sym procnames)])
;          (if (number? pos)
;              (closure (list-ref idss pos)
;                        (list (list-ref bodies pos))
;                        env)
;              (apply-env old-env sym succeed fail)))])))

; This is the new apply-env
(define apply-env
  (lambda (env sym succeed fail) 
    (deref (apply-env-ref env sym succeed fail))))

(define apply-env-ref 
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record ()
        (apply-env-ref global-env sym succeed fail))  ; DERP: So this can possibly infinite loop. Also changed this to global-env
      [extended-env-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
            (if (list? vals)
              (succeed (list-ref vals pos))
              (succeed vals))
            (apply-env-ref env sym succeed fail)))]
      [recursively-extended-env-record
        (procnames idss bodies old-env)
        (let ([pos 
              (list-find-position sym procnames)])
          (if (number? pos)
              (box (closure (list-ref idss pos)
                                      (list (list-ref bodies pos))
                                      env))
              (apply-env-ref old-env sym succeed fail)))])))

(define deref unbox)
(define set-ref! set-box!)