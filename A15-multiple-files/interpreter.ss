; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (trace-lambda 6 (form)
    ; later we may add things that are not expressions.
    ;(display form)
    (eval-exp form init-env)))




(define syntax-expand (trace-lambda 999 (exp)
  (cases expression exp
    [var-exp (id) exp]
    [lit-exp (val) exp]
    [if-exp (test-exp true-exp false-exp)
      (if-exp (syntax-expand test-exp) (syntax-expand true-exp) (syntax-expand false-exp))]
    [if-no-else-exp (test-exp true-exp)
      (if-no-else-exp (syntax-expand test-exp) (syntax-expand true-exp))]
    [lambda-exp (id body)
        (lambda-exp id (map syntax-expand body))]
    [lambda-varlist-exp (id body)
        (lambda-varlist-exp id (map syntax-expand body))]
    [lambda-improperlist-exp (id body)
        (lambda-improperlist-exp id (map syntax-expand body))]
    [while-exp (test bodies)
      (while-exp (syntax-expand test) (map syntax-expand bodies))]
    [begin-exp (body)
        (app-exp (lambda-exp '() (map syntax-expand body)) '())]
    [app-exp (rator rand)
      (app-exp (syntax-expand rator) (map syntax-expand rand))]
    [let-exp (ids rands body)
        (app-exp (syntax-expand (lambda-exp ids body)) (map syntax-expand rands))]
    [let-name-exp (name ids rands body)
        (app-exp (letrec-exp (list name) (list ids) body (parse-exp name)) rands)]  ; DERP: This is totes wrong
    [let*-exp (ids rands body)
        (if (null? ids)
          (app-exp (lambda-exp '() (map syntax-expand body)) '())
          (app-exp (lambda-exp (list (car ids)) (list (syntax-expand (let*-exp (cdr ids) (cdr rands) body)))) (list (car rands))))]
    [cond-exp (conds bodies)
      (let ((body (if (equal? (begin-exp '()) (car bodies)) (lit-exp '#t) (car bodies))))
        (cond
          [(and (= (length conds) 1) (not (eq? (cadar conds) 'else)))
            (if-no-else-exp (syntax-expand (car conds)) (syntax-expand body))]
          [(eq? (cadar conds) 'else) 
            (syntax-expand (car bodies))]
          [(and (= (length conds) 2) (equal? (cadadr conds) 'else))
            (if-exp (syntax-expand (car conds)) (syntax-expand body) (syntax-expand (cadr bodies)))]
          [else
            (if-exp (syntax-expand (car conds)) (syntax-expand body)
              (syntax-expand (cond-exp (cdr conds) (cdr bodies))))]))]
    [set!-exp (id val)
      (set!-exp id (syntax-expand val))]
    [letrec-exp (proc-name ids rands body)
      (letrec-exp proc-name ids (map syntax-expand rands) (syntax-expand body))]
    [and-exp (conds)
      (if (null? conds)
        (lit-exp '#t)
        (if (= (length conds) 1)
          (if-exp (syntax-expand (car conds)) (car conds) (lit-exp '#f))
          (if-exp (syntax-expand (car conds)) (syntax-expand (and-exp (cdr conds))) (lit-exp '#f))))]
    [or-exp (conds)
      (if (null? conds)
        (lit-exp '#f)
        (if (= (length conds) 1)
          (if-exp (syntax-expand (car conds)) (car conds) (lit-exp '#f))
          (if-exp (syntax-expand (car conds)) (car conds) (syntax-expand (or-exp (cdr conds))))))]
    [case-exp (test keys bodies)
      (let ((tester ((lambda (test)
            (lambda (key)
              (app-exp (var-exp 'eq?) (list (syntax-expand test) (syntax-expand key)))
            )
          ) test)))
        (let ((test-exp (syntax-expand (or-exp (map tester (car keys))))))
          (cond
            [(and (= (length keys) 1) (not (eq? (car (cdr (caar keys))) 'else)))
              (if-no-else-exp test-exp (syntax-expand (car bodies)))]
            [(eq? (car (cdr (caar keys))) 'else) 
              (syntax-expand (car bodies))]
            [(and (= (length keys) 2) (equal? (cdadar keys) 'else))
              (if-exp test-exp (syntax-expand (car bodies)) (syntax-expand (cadr bodies)))]
            [else
              (if-exp test-exp (syntax-expand (car bodies))
                (syntax-expand (case-exp test (cdr keys) (cdr bodies))))])))])))




; eval-exp is the main component of the interpreter

(define eval-exp
  (trace-lambda 7 (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
        (apply-env env id; look up its value.
          (trace-lambda 8 (x) x) ; procedure to call if id is in the environment 
          (trace-lambda 9 ()
            (apply-env init-env id ; Eventually we want to change init-env to global-env to have a better name
              (trace-lambda 10 (x) x)
              (eopl:error 'apply-env ; procedure to call if id not in env
                "variable not found in environment: ~s"
                  id))
           ))] 
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]
      [let-exp (ids exprs bodies)
        (let ([new-env
                (extend-env ids
                            (map (trace-lambda 11 (x) (eval-exp x env))
                              exprs)
                            env)])
          (let loop ([bodies bodies])
            (if (null? (cdr bodies))
                (eval-exp (car bodies) new-env)
                (begin (eval-exp (car bodies) new-env)
                       (loop (cdr bodies))))))]
      [letrec-exp (proc-names ids exprs letrec-body)
        (eval-exp letrec-body
          (extend-env-recursively
            proc-names ids exprs env))]

      [if-exp (test-exp then-exp else-exp)
        (if (eval-exp test-exp env)
            (eval-exp then-exp env)
            (eval-exp else-exp env))]
      [if-no-else-exp (test-exp then-exp)
        (if (eval-exp test-exp env)
            (eval-exp then-exp env))]
      [lambda-exp (args body)
        (closure args body env)]
      [lambda-varlist-exp (arg body)
        (closure-list arg body env)]
      [lambda-improperlist-exp (arg body)
        (closure-improperlist arg body env)]
      [while-exp (test bodies)
        (if (eval-exp test env)
          (begin 
            (map eval-exp bodies (list-of-items env (length bodies)))
            (eval-exp (while-exp test bodies) env)))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

(define list-of-items 
  (lambda (x n)
    (if (= n 0)
      '()
      (append (list x) (list-of-items x (- n 1))))))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (trace-lambda 12 (rands env)
    (map (trace-lambda 13 (x) (eval-exp x env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (trace-lambda 14 (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases
      [closure (ids body env)
        (let ([new-env (extend-env ids args env)])  ; DERP: It's not working. 
          (let looping ((body body)) 
            (if (null? (cdr body))
              (eval-exp (car body) new-env) 
              (begin 
                (eval-exp (car body) new-env) 
                (looping (cdr body))))))]
      [closure-list (ids body env)
        (let ([new-env (extend-env (list ids) (list args) env)])
          (let looping ((body body)) 
            (if (null? (cdr body))
              (eval-exp (car body) new-env) 
              (begin 
                (eval-exp (car body) new-env) 
                (looping (cdr body))))))]
      [closure-improperlist (ids body env)
        (let ([new-env (extend-env (improperlist-to-list ids) (improperlist-helper-args ids args) env)])
          (let looping ((body body)) 
            (if (null? (cdr body))
              (eval-exp (car body) new-env) 
              (begin 
                (eval-exp (car body) new-env) 
                (looping (cdr body))))))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define improperlist-to-list 
  (lambda (parameters)
    (if (symbol? (cdr parameters))
        (list (car parameters) (cdr parameters))
        (cons (car parameters) (improperlist-to-list (cdr parameters))))))
(define improperlist-helper-args 
  (lambda (improper-list args)
    (if (symbol? (cdr improper-list))
        (list (car args) (cdr args))
        (cons (car args) (improperlist-helper-args (cdr improper-list) (cdr args))))))

(define *prim-proc-names* '(+ - * add1 sub1 cons = / zero? not < <= > >=
        car cdr list null? assq eq? eqv? equal? atom? length list-vector list? 
        pair? procedure? vector->list list->vector vector make-vector vector-ref vector? number? 
        symbol? set-car! set-cdr! vector-set! display newline map apply caar cadr cdar 
        cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr quotient))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (trace-lambda 15 (prim-proc args)
    (case prim-proc
      [(+) (apply-and-check-args + args 0 >=)]
      [(-) (apply-and-check-args - args 1 >=)]
      [(*) (apply-and-check-args * args 0 >=)]
      [(add1) (if (= (length args) 1) 
                (+ (1st args) 1) 
                (error 'apply-prim-proc 
                  "Wrong number of arguments to add1, Received: ~s" 
                  (length args)))]
      [(sub1) (if (= (length args) 1) 
                (- (1st args) 1) 
                (error 'apply-prim-proc 
                  "Wrong number of arguments to sub1, Received: ~s" 
                  (length args)))]
      [(cons) (apply-and-check-args cons args 2 =)]
      [(=) (apply-and-check-args = args 1 >=)]
      [(/) (apply-and-check-args / args 1 >=)]
      [(zero?) (apply-and-check-args zero? args 1 =)]
      [(not) (apply-and-check-args not args 1 =)]
      [(<) (apply-and-check-args < args 1 >=)]
      [(<=) (apply-and-check-args <= args 1 >=)]
      [(>) (apply-and-check-args > args 1 >=)]
      [(>=) (apply-and-check-args >= args 1 >=)]
      [(car) (apply-and-check-args car args 1 =)]
      [(cdr) (apply-and-check-args cdr args 1 =)]
      [(list) args]
      [(null?) (apply-and-check-args null? args 1 =)]
      [(assq) (apply-and-check-args assq args 2 =)]
      [(eq?) (apply-and-check-args eq? args 2 =)]
      [(equal?) (apply-and-check-args equal? args 2 =)]
      [(eqv?) (apply-and-check-args eqv? args 2 =)]
      [(atom?) (apply-and-check-args atom? args 1 =)]
      [(length) (apply-and-check-args length args 1 =)]
      [(list->vector) (apply-and-check-args list->vector args 1 =)]
      [(list?) (apply-and-check-args list? args 1 =)]
      [(pair?) (apply-and-check-args pair? args 1 =)]
      [(procedure?) (if (pair? args) (apply-and-check-args proc-val-er? args 1 >=) #f)]
      [(vector->list) (apply-and-check-args vector->list args 1 =)]
      [(vector) (apply-and-check-args vector args 0 >=)]
      [(make-vector) (apply-and-check-args make-vector args 1 =)]
      [(vector-ref) (apply-and-check-args vector-ref args 2 =)]
      [(vector?) (apply-and-check-args vector? args 1 =)]
      [(number?) (apply-and-check-args number? args 1 =)]
      [(symbol?) (apply-and-check-args symbol? args 1 =)]
      [(set-car!) (apply-and-check-args set-car! args 2 =)]
      [(set-cdr!) (apply-and-check-args set-cdr! args 2 =)]
      [(vector-set!) (apply-and-check-args vector-set! args 3 =)]
      [(map) (apply-and-check-args our-map args 2 >=)]
      [(apply) (apply-and-check-args
                  (lambda (proc ls . lsts)
                    (apply-proc proc
                      (flatten-apply
                        (if (null? lsts) ls (cons ls lsts))))) args 2 >=)]
      [(caar) (apply-and-check-args caar args 1 =)]
      [(cadr) (apply-and-check-args cadr args 1 =)]
      [(cdar) (apply-and-check-args cdar args 1 =)]
      [(cddr) (apply-and-check-args cddr args 1 =)]
      [(caaar) (apply-and-check-args caaar args 1 =)]
      [(caadr) (apply-and-check-args caadr args 1 =)]
      [(cadar) (apply-and-check-args cadar args 1 =)]
      [(caddr) (apply-and-check-args caddr args 1 =)]
      [(cdaar) (apply-and-check-args cdaar args 1 =)]
      [(cdadr) (apply-and-check-args cdadr args 1 =)]
      [(cddar) (apply-and-check-args cddar args 1 =)]
      [(cdddr) (apply-and-check-args cdddr args 1 =)]
      [(newline) (newline)]
      [(display) (apply display args)]
      [(quotient) (quotient (1st args) (2nd args))]
      [(append) (apply-and-check-args append args 2 >=)]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))

(define (flatten-apply ls)
  (let flatten ((ls ls))
    (cond ((null? (cdr ls))
            (if (list? (car ls))
                (car ls)
                ls))
          (else (cons (car ls) (flatten (cdr ls)))))))

(define our-map
  (lambda (proc lst . lsts)
    (if (null? lsts)
        (let map1 ((ls lst))
          (if (null? ls)
              '()
              (cons (apply-proc proc (car ls))
                    (map1 (cdr ls)))))
        (let map-more ((ls lst) (lsts lsts))
          (if (null? ls)
              '()
              (cons (apply-proc proc (cons (car ls) (our-map (prim-proc 'car) (list lsts))))
                    (map-more (cdr ls)
                              (our-map (prim-proc 'cdr) (list lsts)))))))))

;Checks argument count 
(define check-arg-count 
  (trace-lambda 16 (args num check) 
    (check (length args) num)))

;First checks the argument number then applies given procedure
; to the argument list if it is right number of arguments.
(define apply-and-check-args 
  (trace-lambda 17 (proc args num check) 
    (if (check-arg-count args num check) 
      (apply proc args) 
      (error 'apply-built-in-proc 
        "Wrong number of arguments for proc: ~s Needs ~s ~s Received ~s"
         proc check num (length args)))))

(define rep      ; "read-eval-print" loop.
  (trace-lambda 18 ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (trace-lambda 19 (x) (top-level-eval (syntax-expand (parse-exp x)))))










