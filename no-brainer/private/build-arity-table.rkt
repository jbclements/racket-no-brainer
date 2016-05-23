(module build-arity-table racket/base
  
  (require syntax/kerncase
           racket/contract
           "arity-table.rkt")
  
  (provide/contract [build-arity-table (-> syntax? table?)])
  
  (define (build-arity-table stx)
    (coalesce-table (top-level-expr-iterator stx)))
  

  ;; TEMPLATE FUNCTIONS:
  ;;  these functions' definitions follow the data definitions presented in the Syntax
  ;;  chapter of the MzScheme Manual. 
  
  (define (top-level-expr-iterator stx)
    (kernel-syntax-case stx #f
      [(module identifier name (#%plain-module-begin . module-level-exprs))
       (apply append (map module-level-expr-iterator (syntax->list #'module-level-exprs)))]
      ;; untested:
      [(#%expression expr)
       (expr-iterator #'expr #f)]
      ;; untested:
      [(begin . top-level-exprs)
       (apply append (map top-level-expr-iterator (syntax->list #'top-level-exprs)))]
      [else-stx
       (general-top-level-expr-iterator stx)]))

  (define (module-level-expr-iterator stx)
    (kernel-syntax-case stx #f
      [(#%provide . provide-specs)
       null]
      [else-stx
       (general-top-level-expr-iterator stx)]))
  
  (define (general-top-level-expr-iterator stx)
    (kernel-syntax-case stx #f
      [(define-values (var ...) expr)
       (let ([var-list (syntax->list #'(var ...))])
         (cond [(= (length var-list) 1) (expr-iterator #'expr (car var-list))]
               [else (expr-iterator #'expr #f)]))]
      [(define-syntaxes (var ...) expr)
       null]
      ;; untested:
      [(define-values-for-syntax (id ...) expr)
       null]
      ;; untested:
      [(#%require . require-specs)
       null]
      [else
       (expr-iterator stx #f)]))
 
  ; a note about the use of the term 'tail':  for the purposes of this function,
  ; a 'tail' position is one whose value is *guaranteed* be the result of this 
  ; expression.  This is to protect soundness.
  
  (define (expr-iterator stx potential-name)
    (let* ([recur-tail (lambda (expr) (expr-iterator expr potential-name))]
           [recur-non-tail (lambda (expr) (expr-iterator expr #f))]
           [recur-with-name (lambda (expr name) (expr-iterator expr name))]
           [lambda-clause-abstraction
            (lambda (clause)
              (kernel-syntax-case clause #f
                [(arglist . bodies)
                 (let ([rest (apply append (map recur-non-tail (syntax->list #'bodies)))])
                   (if potential-name
                       (cons
                        (list potential-name (list (arity-of-arglist #'arglist)))
                        rest)
                       rest))]
                [else
                 (error 'expr-syntax-object-iterator 
                        "unexpected (case-)lambda clause: ~a" 
                        (syntax->datum stx))]))]
           [let-values-abstraction
            (lambda (stx)
              (kernel-syntax-case stx #f
                [(kwd (((variable ...) rhs) ...) . bodies)
                 (let* ([clause-fn 
                         (lambda (vars rhs)
                           (let ([var-list (syntax->list vars)])
                             (cond [(= (length var-list) 1) 
                                    (recur-with-name rhs (car var-list))]
                                   [else
                                    (recur-non-tail rhs)])))])
                   (apply append
                    (append (map clause-fn (syntax->list #'((variable ...) ...)) (syntax->list #'(rhs ...)))
                            (map recur-non-tail (syntax->list #'bodies)))))]
                [else
                 (error 'expr-syntax-object-iterator 
                        "unexpected let(rec) expression: ~a"
                        stx
                        ;(syntax-object->datum stx)
                        )]))]) 
         (kernel-syntax-case stx #f
           [var-stx
            (identifier? (syntax var-stx))
            null]
           [(#%plain-lambda . clause)
            (lambda-clause-abstraction #'clause)]
           [(case-lambda . clauses)
            (apply append (map lambda-clause-abstraction (syntax->list #'clauses)))]
           [(if test then else)
            (append
             (recur-non-tail #'test)
             (recur-non-tail #'then)
             (recur-non-tail #'else))]
           [(begin . bodies)
            (let ([body-list (syntax->list #'bodies)])
              (apply append
                     (recur-tail (car (reverse body-list)))
                     (map recur-non-tail (reverse (cdr (reverse body-list))))))]
           [(begin0 . bodies)
            (let ([body-list (syntax->list #'bodies)])
              (apply append
                     (recur-tail (car body-list))
                     (map recur-non-tail (cdr body-list))))]
           [(let-values . _)
            (let-values-abstraction stx)]
           [(letrec-values . _)
            (let-values-abstraction stx)]
           [(set! var val)               
            (cons (list #'var `(unknown))
                  (recur-non-tail #'val))]
           [(quote _)
            null]
           [(quote-syntax _)
            null]
           [(with-continuation-mark key mark body)
            (append
             (recur-non-tail #'key)
             (recur-non-tail #'mark)
             (recur-tail #'body))]
           [(#%plain-app . exprs)
            (apply append (map recur-non-tail (syntax->list #'exprs)))]
           [(#%top . var)
            null]
           [(#%variable-reference id)
            null]
           [(#%variable-reference (#%top . id))
            null]
           [(#%variable-reference)
            null]
           [else
            (error 'expr-iterator "unknown expr: ~a" 
                   (syntax->datum stx))])))
  
  (define (arity-of-arglist arglist-stx)
    (syntax-case arglist-stx ()
      [var
       (identifier? arglist-stx)
       (list 0 'inf)]
      [(var ...)
       (let ([args (length (syntax->list #'(var ...)))])
         (list args args))]
      [(var . others)
       (let ([arity-of-rest (arity-of-arglist #'others)])
         (list (incr-limit (car arity-of-rest))
               (incr-limit (cadr arity-of-rest))))]))
  
  (define incr-limit
    (contract 
     (-> (or/c number? (symbols 'inf)) any)
     (lambda (limit)
       (cond [(number? limit) (+ 1 limit)]
             [(eq? limit 'inf) 'inf]))
     'incr-limit
     'caller))


 
  
;   ;this fails (I believe) because that big long symbol is uninterned.
;   (require (lib "class.ss"))
;  (build-arity-table-test `((my-mixin ((1 1))) 
;                            (|a method in #<struct:object:/Users/clements/plt/collects/stepper/stepper-tool.ss:479:9>:222:31| ((2 2))))
;                          #'(define (my-mixin super%)
;                              (class super% ()
;                                (define/public (a x) x)
;                                (super-instantiate ()))))

  )



