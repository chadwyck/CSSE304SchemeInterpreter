;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process

(load "chez-init.ss") 



;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+



;; Parsed expression datatypes

(define-datatype expression expression?
  [var-exp        ; variable references
   (id symbol?)]
  [lit-exp        ; "Normal" data.  Did I leave out any types?
   (datum
    (lambda (x)
      (ormap 
       (lambda (pred) (pred x))
       (list number? vector? boolean? symbol? string? pair? null?))))]
  [app-exp        ; applications
   (rator expression?)
   (rands (list-of expression?))]  
  )

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure (params (list-of symbol?))
            (body expression?)
            (env environment?)])
	 
	 
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))
;; Parsed expression datatypes

(define-datatype expression expression?
  [var-exp        ; variable references
   (id symbol?)]
  [lit-exp        ; "Normal" data.  Did I leave out any types?
   (datum
    (lambda (x)
      (ormap 
       (lambda (pred) (pred x))
       (list number? vector? boolean? symbol? string? pair? null?))))]
  [app-exp        ; applications
   (rator expression?)
   (rands (list-of expression?))]  
  )
	 
	 
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))






;-------------------+
;                   |
;    PARSER         |
;                   |
;-------------------+


; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(list? datum)
        (cond [(eqv? (car datum) 'lambda)
                (cond [(null? (cddr datum))
                        (eopl:error 'parse-exp
                          "lambda-expression: incorrect length ~s" datum)]
                      [(and (not ((list-of symbol?) (cadr datum))) (not (symbol? (cadr datum))))
                        (eopl:error 'parse-exp
                          "lambda's formal arguments ~s must all be symbols" (cadr datum))]
                      [((list-of symbol?) (cadr datum))
                       (lambda-exp (cadr datum)
                        (map parse-exp (cddr datum)))]
                      [else
                        (lambda-varlist-exp (cadr datum)
                          (map parse-exp (cddr datum)))])]
              [(eqv? (car datum) 'let)
                (cond [(symbol? (cadr datum))
                        (cond [(< (length datum) 4)
                                (eopl:error 'parse-exp
                                  "~s-expression has incorrect length ~s" datum)]
                              [(not ((list-of list?) (caddr datum)))
                                (eopl:error 'parse-exp
                                  "declarations in ~s-expression not a list ~s" (car datum) datum)]
                              [(not (andmap (lambda (x) (equal? 2 (length x))) (caddr datum)))
                                (eopl:error 'parse-exp
                                  "declaration in ~s-exp must be a list of length 2 ~s" (car datum) datum)]
                              [(not (andmap (lambda (x) (symbol? (car x))) (caddr datum)))
                                (eopl:error 'parse-exp
                                  "first members must be symbols: ~s" (caddr datum))]
                              [else
                                (let-name-exp (cadr datum) (map parse-exp (caddr datum)) (map parse-exp (cdddr datum)))])]
                      [(list? (cadr datum))
                        (cond [(< (length datum) 3)
                                (eopl:error 'parse-exp
                                  "~s-expression has incorrect length ~s" (car datum) datum)]
                              [(not ((list-of list?) (cadr datum)))
                                (eopl:error 'parse-exp
                                  "declarations in ~s-expression not a list ~s" (car datum) datum)]
                              [(not (andmap (lambda (x) (equal? 2 (length x))) (cadr datum)))
                                (eopl:error 'parse-exp
                                  "declaration in ~s-exp must be a list of length 2 ~s" (car datum) datum)]
                              [(not (andmap (lambda (x) (symbol? (car x))) (cadr datum)))
                                (eopl:error 'parse-exp
                                  "first members must be symbols: ~s" (cadr datum))]
                              [else
                                (let-exp (map parse-exp (cadr datum)) (map parse-exp (cddr datum)))])]
                      [else
                        (eopl:error 'parse-exp
                          "declarations in ~s-expression not a list ~s" (car datum) datum)])]
              [(eqv? (car datum) 'let*)
                (cond [(list? (cadr datum))
                        (cond [(< (length datum) 3)
                                (eopl:error 'parse-exp
                                  "~s-expression has incorrect length ~s" (car datum) datum)]
                              [(not ((list-of list?) (cadr datum)))
                                (eopl:error 'parse-exp
                                  "declarations in ~s-expression not a list ~s" (car datum) datum)]
                              [(not (andmap (lambda (x) (equal? 2 (length x))) (cadr datum)))
                                (eopl:error 'parse-exp
                                  "declaration in ~s-exp must be a list of length 2 ~s" (car datum) datum)]
                              [(not (andmap (lambda (x) (symbol? (car x))) (cadr datum)))
                                (eopl:error 'parse-exp
                                  "first members must be symbols: ~s" (cadr datum))]
                              [else
                                (let*-exp (map parse-exp (cadr datum)) (map parse-exp (cddr datum)))])]
                      [else
                        (eopl:error 'parse-exp
                          "declarations in ~s-expression not a list ~s" (car datum) datum)])]
              [(eqv? (car datum) 'letrec)
                (cond [(list? (cadr datum))
                        (cond [(< (length datum) 3)
                                (eopl:error 'parse-exp
                                  "~s-expression has incorrect length ~s" (car datum) datum)]
                              [(not ((list-of list?) (cadr datum)))
                                (eopl:error 'parse-exp
                                  "declarations in ~s-expression not a list ~s" (car datum) datum)]
                              [(not (andmap (lambda (x) (equal? 2 (length x))) (cadr datum)))
                                (eopl:error 'parse-exp
                                  "declaration in ~s-exp must be a list of length 2 ~s" (car datum) datum)]
                              [(not (andmap (lambda (x) (symbol? (car x))) (cadr datum)))
                                (eopl:error 'parse-exp
                                  "first members must be symbols: ~s" (cadr datum))]
                              [else
                                (letrec-exp (map parse-exp (cadr datum)) (map parse-exp (cddr datum)))])]
                      [else
                        (eopl:error 'parse-exp
                          "declarations in ~s-expression not a list ~s" (car datum) datum)])]
              [(eqv? (car datum) 'if)
                (cond [(or (< (length datum) 3) (> (length datum) 4))
                        (eopl:error 'parse-exp
                          "if-expression ~s does not have (only) test, then, and else" datum)]
                      [(equal? (length datum) 3)
                        (if-no-else-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)))]
                      [(equal? (length datum) 4)
                        (if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)) (parse-exp (cadddr datum)))])]
              [(eqv? (car datum) 'set!)
                (cond [(not (equal? 3 (length datum)))
                        (eopl:error 'parse-exp
                          "set! expression: ~s is wrong length" datum)]
                      [else
                        (set!-exp (cadr datum) (parse-exp (caddr datum)))])]
              ;[(lit-exp? datum) (lit-exp datum)]
              [else
                (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))])]
      [(lit-exp? datum) (lit-exp datum)]
      [else (eopl:error 'parse-exp
              "Invalid concrete syntax ~s" datum)])))








;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+





; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

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

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record ()
        (fail))
      (extended-env-record (syms vals env)
	(let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	      (succeed (list-ref vals pos))
	      (apply-env env sym succeed fail)))))))








;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+



; To be added later









;-------------------+
;                   |
;   INTERPRETER    |
;                   |
;-------------------+



; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form)))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
				(apply-env env id; look up its value.
          (lambda (x) x) ; procedure to call if id is in the environment 
          (lambda ()
            (apply-env init-env id ; Eventually we want to change init-env to global-env to have a better name
              (lambda (x) x)
              (eopl:error 'apply-env ; procedure to call if id not in env
                "variable not found in environment: ~s"
                  id))
           ))] 
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator)]
              [args (eval-rands rands)])
          (apply-proc proc-value args))]
      [let-exp (ids exprs bodies)
        (let ([new-env
                (extend-env ids
                            (map (lambda (x) (eval-exp x env))
                              exprs)
                            env)])
          (let loop ([bodies bodies])
            (if (null? (cdr bodies))
                (eval-exp (car bodies) new-env)
                (begin (eval-exp (car bodies) new-env)
                       (loop (cdr bodies))))))]
      [letrec-exp (proc-names idss bodies letrec-body)
        (eval-exp letrec-body
          (extend-env-recursively
            proc-names idss bodies env))]
      [if-exp (test-exp then-exp else-exp)
        (if (eval-exp test-exp env)
            (eval-exp then-exp env)
            (eval-exp else-exp env))]
      [lambda-exp (args body)
        (closure args body env)]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands)
    (map eval-exp rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases
      [closure (params body env)
        (let ([extended-env (extend-env params
                                        args
                                        env)])
        (eval-exp body extended-env))]

      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * add1 sub1 cons =))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (+ (1st args) (2nd args))]
      [(-) (- (1st args) (2nd args))]
      [(*) (* (1st args) (2nd args))]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (= (1st args) (2nd args))]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))

(define extend-env-recursively 
  (lambda (proc-names idss bodies old-env)
    (let ([len (length proc-names)])
      (let ([vec (make-vector len)])
        (let ([env (extended-env-record proc-names vec old-env)])
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len)
            idss
            bodies
          )
        env)))))







