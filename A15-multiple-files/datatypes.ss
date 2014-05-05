
;; Parsed expression datatypes
(define lit-exp?
  (lambda (id) (or (number? id) (string? id) (list? id) (symbol? id)
                         (boolean? id) (vector? id))))
(define-datatype expression expression?
  [var-exp        ; variable references
   (id symbol?)]
  [lit-exp        ; "Normal" data.  Did I leave out any types?
   (datum
    (lambda (x)
      (ormap 
       (lambda (pred) (pred x))
       (list number? vector? boolean? symbol? string? pair? null?))))]
  [lambda-exp
      (id (list-of symbol?))
      (list-of-bodies (list-of expression?))]
  [lambda-varlist-exp
    (id symbol?)
    (list-of-bodies (list-of expression?))]
  [let-exp
    (list-of-app (list-of expression?))
    (list-of-bodies (list-of expression?))]
  [let-name-exp
    (name expression?)
    (list-of-app (list-of expression?))
    (list-of-bodies (list-of expression?))]
  [let*-exp
    (list-of-app (list-of expression?))
    (list-of-bodies (list-of expression?))]
  [letrec-exp
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
)

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)])
	 
	 
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))