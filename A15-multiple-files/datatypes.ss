
;; Parsed expression datatypes
(define lit-exp?
  (trace-lambda 0 (x)
      (ormap 
       (trace-lambda 1 (pred) (pred x))
       (list number? vector? boolean? symbol? string? 
          (trace-lambda 2 (x)
            (if (pair? x)
              (if (equal? (car x) 'quote)
                #t #f)
              #f))
          null?))))
(define improper-list?
  (trace-lambda 37 (x)
    (and (not (list? x)) (pair? x))))
(define-datatype expression expression?
  [var-exp        ; variable references
   (id symbol?)]
  [lit-exp        ; "Normal" data.  Did I leave out any types?
   (datum
    (trace-lambda 3 (x)
      (ormap 
       (trace-lambda 4 (pred) (pred x))
       (list number? vector? boolean? symbol? string? pair? null?))))]
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
    (name expression?)
    (list-of-ids (list-of symbol?))
    (list-of-app (list-of expression?))
    (list-of-bodies (list-of expression?))]
  [let*-exp
    (list-of-ids (list-of symbol?))
    (list-of-app (list-of expression?))
    (list-of-bodies (list-of expression?))]
  [letrec-exp
    (list-of-proc-names (list-of expression?))
    (list-of-ids (list-of expression?))
    (list-of-app (list-of expression?))
    (list-of-bodies (list-of expression?))]
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
)

;; environment type definitions

(define scheme-value?
  (trace-lambda 5 (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))




; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
    (args (list-of symbol?))
    (body (list-of expression?))
    (env environment?)]
  [closure-list
    (args symbol?)
    (body (list-of expression?))
    (env environment?)]
  [closure-improperlist
    (args improper-list?)
    (body (list-of expression?))
    (env environment?)])
	
