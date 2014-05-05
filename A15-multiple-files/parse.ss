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
      [(lit-exp? datum) (lit-exp (if (pair? datum) (if (equal? (car datum) 'quote) (cadr datum) datum) datum))]
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
      [else (eopl:error 'parse-exp
              "Invalid concrete syntax ~s" datum)])))










