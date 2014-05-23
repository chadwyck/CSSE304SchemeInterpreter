(define-datatype continuation continuation?
  [init-k]
  [test-k
    (then-exp expression?)
    (else-exp expression?)
    (env environment?)
    (k continuation?)]
  [test-no-else-k
    (then-exp expression?)
    (env environment?)
    (k continuation?)]
  [rator-k (rands (list-of expression?))
           (env environment?)
           (k continuation?)]
  [rands-k (proc-value scheme-value?)
          (k continuation?)]
  [map-k (proc scheme-value?)
          (ls list?)
          (k continuation?)]
  [cons-k (val scheme-value?)
          (k continuation?)]
  [eval-rands-k
    (env environment?)
    (rands (list-of expression?))
    (k continuation?)]
)

(define (apply-k k v)
  (cases continuation k
    [init-k () v]
    [test-k (then-exp else-exp env k)
      (if v
          (eval-exp then-exp env k)
          (eval-exp else-exp env k))]
    [test-no-else-k (then-exp env k)
      (if v
          (eval-exp then-exp env k))]
    [rator-k (rands env k)
             (eval-rands rands
                         env
                         (rands-k v k))]
    [rands-k (proc-value k)
             (apply-proc (deref proc-value) v k)]
    [map-k (proc ls k)
          (map-cps proc ls (cons-k v k))]
    [cons-k (val k)
          (apply-k k (cons val v))]
    [eval-rands-k (env rands k)
                  (eval-rands rands env (cons-k v k))]
    ))