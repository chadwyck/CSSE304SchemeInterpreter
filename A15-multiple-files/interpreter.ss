; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (trace-lambda 6 (form)
    ; later we may add things that are not expressions.
    ;(display form)
    (eval-exp form init-env)))

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
      [letrec-exp (proc-names idss bodies letrec-body)
        (eval-exp letrec-body
          (extend-env-recursively
            proc-names idss bodies env))]
      [if-exp (test-exp then-exp else-exp)
        (if (eval-exp test-exp env)
            (eval-exp then-exp env)
            (eval-exp else-exp env))]
      [if-no-else-exp (test-exp then-exp)
        (if (eval-exp test-exp env)
            (eval-exp then-exp env))]
      [lambda-exp (args body)
        (closure args body env)]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

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
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * add1 sub1 cons = / zero? not < <= > >=
        car cdr list null? assq eq? equal? atom? length list-vector list? 
        pair? procedure? vector->list list->vector vector make-vector vector-ref vector? number? 
        symbol? set-car! set-cdr! vector-set! display newline caar cadr cdar 
        cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr))

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
      [(atom?) (apply-and-check-args atom? args 1 =)]
      [(length) (apply-and-check-args length args 1 =)]
      [(list->vector) (apply-and-check-args list->vector args 1 =)]
      [(list?) (apply-and-check-args list? args 1 =)]
      [(pair?) (apply-and-check-args pair? args 1 =)]
      [(procedure?) (apply-and-check-args proc-val? args 1 =)]
      [(vector->list) (apply-and-check-args vector->list args 1 =)]
      [(vector) (apply-and-check-args args 0 >=)]
      [(make-vector) (apply-and-check-args make-vector args 1 =)]
      [(vector-ref) (apply-and-check-args vector-ref args 2 =)]
      [(vector?) (apply-and-check-args vector? args 1 =)]
      [(number?) (apply-and-check-args number? args 1 =)]
      [(symbol?) (apply-and-check-args symbol? args 1 =)]
      [(set-car!) (apply-and-check-args set-car! args 2 =)]
      [(set-cdr!) (apply-and-check-args set-cdr! args 2 =)]
      [(vector-set!) (apply-and-check-args vector-set! args 3 =)]
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
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))

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
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (trace-lambda 19 (x) (top-level-eval (parse-exp x))))










