#lang racket/base
  
(require syntax/kerncase
         racket/contract
         "arity-table.rkt")
;; -----------------------------------------------------------------------------
;; TODO: Replace with id-sets?
(define (set-pair-union a-set b-set comparator)
  (cond [(null? b-set) a-set]
        [(null? a-set) b-set]
        [else (append (remove* a-set b-set comparator) a-set)]))

(define (varref-set-pair-union a-set b-set)
  (set-pair-union a-set b-set free-identifier=?))

(define (pair-union->many-union fn)
  (lambda (args)
    (foldl fn null args)))

(define varref-set-union
  (pair-union->many-union varref-set-pair-union))

(define (binding-set-varref-set-intersect bindings varrefs)
  (cond [(eq? bindings 'all) varrefs]
        [else (filter (lambda (varref)
                        (ormap (lambda (binding)
                                 (bound-identifier=? binding varref))
                               bindings))
                      varrefs)]))
;; -----------------------------------------------------------------------------
  
  (provide/contract [check-program (-> syntax? table? (listof (cons/c symbol? (cons/c syntax? any/c))))])

  (define (check-program stx table) 
    (result-error-messages (top-result-result (top-level-expr-iterator stx table))))
  
  ;; this analysis computes two things simultaneausly: error messages and ids occurring
  
  (define-struct result (error-messages ids-occurring))
  
  ; the empty-result constant.
  (define empty-result (make-result null null))
  
  ; given just one of the three fields, make a result for it.
  (define (make-err-result errors) (make-result errors null))
  (define (make-id-result ids) (make-result null ids))
  
  ;; combine: result? result? -> 
  ; combine: [a,b,c] + [d,e,f] = [a+d,b+e,c+f]
  (define (combine result1 result2)
    (make-result (append (result-error-messages result1)
                         (result-error-messages result2))
                 (varref-set-union (list (result-ids-occurring result1)
                                         (result-ids-occurring result2)))))

  ; a top-result contains a result, a list of defined ids, and a list 
  (define-struct top-result (result defined-ids all-defined-except))
  
  (define empty-top-result (make-top-result empty-result null null))
  
  (define (make-regular-top-result result) (make-top-result result null null))
  (define (make-defined-id-result ids) (make-top-result empty-result ids null))
  (define (make-all-defined-except-result a-d-e) (make-top-result empty-result null a-d-e))
  
  ;; combine-top: top-result? top-result? -> top-result?
  (define (combine-top result1 result2)
    (make-top-result (combine (top-result-result result1)
                              (top-result-result result2))
                     (varref-set-union (list (top-result-defined-ids result1)
                                             (top-result-defined-ids result2)))
                     (append (top-result-all-defined-except result1)
                             (top-result-all-defined-except result2))))
  
  ; check-ids-used : bindings kind syntax? result? -> result?
  ;  make sure that the bindings named by 'sought' are in the list of ids-occurring in the result
  (define (check-ids-used sought kind stx result)
    (combine 
     result
     (let ([not-used (remove* (result-ids-occurring result) sought free-identifier=?)])
       (if (null? not-used)
           empty-result
           (make-err-result `((unused-bindings ,stx ,kind ,not-used)))))))
  
  ;; figure out which identifiers are provided by the module (so that unused ones can be flagged)
  ;; in order to get this code working before Anika comes home, I'm going to punt on this for now:
  (define (scan-provide-spec provide-spec)
    (syntax-case provide-spec ()
      [id
       (identifier? #'id)
       (make-regular-top-result (make-id-result (list #'id)))]
      [(kwd . rest)
       (identifier? #'kwd)
       (let ([recur-on (lambda (picker) (scan-provide-spec (picker (syntax->list #'rest))))])
         (case (syntax-e #'kwd)
           [(for-meta) (recur-on cadr)]
           [(for-syntax for-label protect) (recur-on car)]
           [(rename) (make-regular-top-result (make-id-result (list (cadr (syntax->list #'rest)))))]
           [(struct)
            ; just punt on this for now... there's probably a nice utility function that does this right
            empty-top-result]
           [(all-from all-from-except) empty-top-result]
           [(all-defined prefix-all-defined) (make-all-defined-except-result null)]
           [(all-defined-except)
            (make-all-defined-except-result (syntax->list #'rest))]
           [(prefix-all-defined-except)
            (make-all-defined-except-result (cdr (syntax->list #'rest)))]
           [(expand) (error scan-provide-spec "docs claim that the 'expand' form doesnt generally occur, in: ~a" (syntax->datum provide-spec))]))]      
      [else (error 'scan-provide-spec "expected a syntax list beginning with a keyword, got: ~a" (syntax->datum provide-spec))])
    (kernel-syntax-case provide-spec #f
      [id
       (identifier? #`id)
       (make-regular-top-result (make-id-result (list #`id)))]
      [(struct struct-identifier (field ...))
       ; just punt on this for now... there's probably a nice utility function that does this right
       empty-top-result]
      [(all-from module-name)
       empty-top-result]
      [(all-from-except module-name id ...)
       empty-top-result]
      [(all-defined)
       (make-all-defined-except-result null)]
      [(all-defined-except id ...)
       (make-all-defined-except-result (syntax->list #`(id ...)))]
      [else
       (error 'scan-provide-spec "unknown provide-spec: ~v" (syntax->datum provide-spec))]))

  (define (module-level-checks stx top-result)
    (combine-top
     top-result
     (let ([unused-defines 
            (foldl
             (lambda (unused-defines a-d-e-list)
               (binding-set-varref-set-intersect unused-defines a-d-e-list))
             (remove* (result-ids-occurring (top-result-result top-result)) (top-result-defined-ids top-result)
                      free-identifier=?)
             (top-result-all-defined-except top-result))])
       (if (null? unused-defines)
           empty-top-result
           (make-regular-top-result (make-err-result `((unused-bindings ,stx module ,unused-defines))))))))
  
  
  
  ; arglist-bindings : (syntax? -> (listof syntax?))
  ;  return a list of the names in the arglist
  
  (define (arglist-bindings arglist-stx)
    (syntax-case arglist-stx ()
      [var
       (identifier? arglist-stx)
       (list arglist-stx)]
      [(var ...)
       (syntax->list arglist-stx)]
      [(var . others)
       (cons #'var (arglist-bindings #'others))]))

  

  ;; TEMPLATE FUNCTIONS:
  ;;  these functions' definitions follow the data definitions presented in the Syntax
  ;;  chapter of the MzScheme Manual. 
  
  ;; top-level-expr-iterator : syntax? table? -> top-level-result?
  
  (define (top-level-expr-iterator stx table)
    (kernel-syntax-case stx #f
      [(module identifier name (#%plain-module-begin . module-level-exprs))
       (module-level-checks
        stx
        (foldl combine-top empty-top-result (map (lambda (expr) (module-level-expr-iterator expr table)) (syntax->list #'module-level-exprs))))]
      [(begin . exps)
       (foldl combine-top empty-top-result (map (lambda (expr) (top-level-expr-iterator expr table)) (syntax->list #`exps)))]
      [(#%expression expr)
       (make-regular-top-result (expr-iterator #'expr table))]
      [else-stx
       (general-top-level-expr-iterator #t stx table)]))

  (define (module-level-expr-iterator stx table)
    (kernel-syntax-case stx #f
      [(#%provide . provide-specs)
       (foldl combine-top empty-top-result (map scan-provide-spec (syntax->list #`provide-specs)))]
      [else-stx
       (general-top-level-expr-iterator #f stx table)]))
  
  
  (define (general-top-level-expr-iterator really-top-level? stx table)
    (kernel-syntax-case stx #f
      [(define-values (var ...) expr)
       (combine-top
        (make-defined-id-result (syntax->list #`(var ...)))
        (make-regular-top-result (expr-iterator #'expr table)))]
      [(define-syntaxes (var ...) expr)
       empty-top-result]
      [(#%require . require-specs)
       empty-top-result]
      [(module . body)
       empty-top-result]
      [else
       (make-regular-top-result (expr-iterator stx table))]))
 
  ; expr-iterator : syntax? table? -> result?
  (define (expr-iterator stx table)
    (let* ([recur (lambda (expr) (expr-iterator expr table))]
           [recur-on-pieces (lambda (exprs-stx) (foldl combine empty-result (map recur (syntax->list exprs-stx))))]
           [lambda-clause-abstraction
            (lambda (clause)
              (kernel-syntax-case clause #f
                [(arglist . bodies)
                 (check-ids-used (arglist-bindings #`arglist)
                                 'lambda
                                 stx
                                 (recur-on-pieces #'bodies))]
                [else
                 (error 'expr-syntax-object-iterator 
                        "unexpected (case-)lambda clause: ~a" 
                        (syntax->datum stx))]))]
           [let-values-abstraction
            (lambda (stx)
              (kernel-syntax-case stx #f
                [(kwd (((variable ...) rhs) ...) body ...)
                 ; note: because of the magic of free-identifier=?, we don't need to differentiate
                 ; between let & letrec here:
                 (check-ids-used (apply append (map syntax->list (syntax->list #`((variable ...) ...))))
                                 'let/rec
                                 stx
                                 (recur-on-pieces #'(rhs ... body ...)))]
                [else
                 (error 'expr-syntax-object-iterator 
                        "unexpected let(rec) expression: ~a"
                        stx
                        ;(syntax-object->datum stx)
                        )]))]) 
         (kernel-syntax-case stx #f
           [var-stx
            (identifier? (syntax var-stx))
            (make-id-result (list #`var-stx))]
           [(#%plain-lambda . clause)
            (lambda-clause-abstraction #'clause)]
           [(case-lambda . clauses)
            (foldl combine empty-result (map lambda-clause-abstraction (syntax->list #'clauses)))]
           [(if test then else)
            (recur-on-pieces #'(test then else))]
           [(begin . bodies)
            (recur-on-pieces #'bodies)]
           [(begin0 . bodies)
            (recur-on-pieces #'bodies)]
           [(let-values . _)
            (let-values-abstraction stx)]
           [(letrec-values . _)
            (let-values-abstraction stx)]
           [(set! var val)
            (recur-on-pieces #'(val))]
           [(quote _)
            empty-result]
           [(quote-syntax _)
            empty-result]
           [(with-continuation-mark key mark body)
            (foldl combine empty-result
                   (list (recur #'key)
                         (recur #'mark)
                         (recur #'body)))]
           [(#%plain-app . exprs)
            (let* ([expr-list (syntax->list #'exprs)])
              (combine
               (foldl combine empty-result (map recur expr-list))
               (if (null? expr-list)
                   empty-result
                   (let* ([fn-pos (car expr-list)])
                     (cond [(syntax-case (car expr-list) (#%top)
                              [var
                               (identifier? #'var)
                               (begin (when (eq? (syntax->datum #'var) 'view-controller-go)
                                        (fprintf (current-error-port) "blarg!\n"))
                               #'var)]
                              [(#%top . var)
                               (identifier? #'var)
                               #'var] 
                              [else #f])
                            =>
                            (lambda (var)
                              (let* ([match (find-match var table)])
                                (if match
                                    (if (arity-match (cadr match) (length (cdr expr-list)))
                                        (make-err-result `((application-ok ,stx)))
                                        (make-err-result `((bad-application ,stx ,(cadr match)))))
                                    (make-err-result `((unknown-id-application ,stx))))))]
                           [else
                            (make-err-result `((non-id-application ,stx)))])))))]
           [(#%top . var)
            (make-id-result (list #`var))]
           [(#%variable-reference id)
            (make-id-result (list #`var))]
           [(#%variable-reference (#%top . var))
            (make-id-result (list #`var))]
           [(#%variable-reference)
            empty-result]
           [else
            (error 'expr-iterator "unknown expr: ~a" 
                   (syntax->datum stx))])))
 
