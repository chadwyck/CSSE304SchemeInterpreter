(define-datatype continuation continuation?
  [init-k]
  [test-k
    (then-exp expression?)
    (else-exp expression?)
    (env environment?)
    (k continuation?)]
  [rator-k (rands (list-of expression?))
           (env environment?)
           (k continuation?)]
  [rands-k (proc-value scheme-value?)
          (k continuation?)]
)

(define (apply-k k v)
  (cases continuation k
    [init-k () v]
    [test-k (then-exp else-exp env k)
      (if v
          (eval-exp then-exp env k)
          (eval-exp else-exp env k))]
    [rator-k (rands env k)
             (eval-rands rands
                         env
                         (rands-k v k))]
    [rands-k (proc-value k)
             (apply-proc proc-value v k)]
    ))