
;; Parsed expression datatypes
(define lit-exp?
  (lambda (x)
      (ormap 
       (lambda (pred) (pred x))
       (list number? vector? boolean? symbol? string? 
          (lambda (x)
            (if (pair? x)
              (if (equal? (car x) 'quote)
                #t #f)
              #f))
          null?))))
(define improper-list?
  (lambda (x)
    (and (not (list? x)) (pair? x))))

(define-datatype expression expression?
  [var-exp        ; variable references
   (id symbol?)]
  [lit-exp        ; "Normal" data.  Did I leave out any types?
   (datum
    (lambda (x)
      (ormap 
       (lambda (pred) (pred x))
       (list number? vector? boolean? symbol? string? pair? null?))))]
  [define-exp
   (var symbol?)
   (body expression?)]
  [lambda-exp
      (id (list-of symbol?))
      (list-of-bodies (list-of expression?))]
  [lambda-varlist-exp
    (id symbol?)
    (list-of-bodies (list-of expression?))]
  [lambda-improperlist-exp
    (id improper-list?)
    (list-of-bodies (list-of expression?))]
  [let-exp
    (list-of-ids (list-of symbol?))
    (list-of-app (list-of expression?))
    (list-of-bodies (list-of expression?))]
  [let-name-exp
    (name symbol?)
    (list-of-ids (list-of symbol?))
    (list-of-app (list-of expression?))
    (list-of-bodies (list-of expression?))]
  [let*-exp
    (list-of-ids (list-of symbol?))
    (list-of-app (list-of expression?))
    (list-of-bodies (list-of expression?))]
  [letrec-exp
    (proc-names (list-of symbol?))
    (idss (list-of (list-of symbol?)))
    (bodies (list-of expression?))
    (letrec-body expression?)]
  [app-exp
    (rator expression?)
    (rand (list-of expression?))]
  [if-exp
    (pred expression?)
    (consequence1 expression?)
    (consequence2 expression?)]
  [if-no-else-exp
    (pred expression?)
    (consequence expression?)]
  [set!-exp
    (var symbol?)
    (expr expression?)]
  [cond-exp 
    (conditions (list-of expression?)) 
    (bodies (list-of expression?))]
  [and-exp 
    (conditions (list-of expression?))]
  [or-exp 
    (conditions (list-of expression?))]
  [case-exp 
    (test-exp expression?) 
    (keys (list-of (lambda (x) (or (list-of expression? x) (equal? 'else (cadr x)))))) 
    (bodies (list-of expression?))]
  [while-exp
    (test expression?)
    (bodies (list-of expression?))]
  [begin-exp
    (bodies (list-of expression?))]
  [call/cc-exp
    (rec expression?)]
)

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (lambda (x) (or ((list-of scheme-value?) x) (scheme-value? x))))
   (env environment?))
  [recursively-extended-env-record
    (proc-names (list-of symbol?))
    (idss (list-of (list-of symbol?)))
    (bodies (list-of expression?))
    (env environment?)])




; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
    (args (list-of symbol?))
    (body (list-of expression?))
    (env scheme-value?)]
  [closure-list
    (args symbol?)
    (body (list-of expression?))
    (env environment?)]
  [closure-improperlist
    (args improper-list?)
    (body (list-of expression?))
    (env environment?)]
  [cont
    (k continuation?)]
  )
	
(define proc-val-er?
  (lambda args
    (if (null? (cdr args))
      (proc-val? (car args))
      (let ((a (car args)))
        (or (equal? a 'prim-proc)
                (equal? a 'closure)
                (equal? a 'closure-list)
                (equal? a 'closure-improperlist))))))