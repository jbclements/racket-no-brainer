#lang scheme

(require "build-arity-table.rkt"
         "check-program.rkt"
         rackunit)

(define-binary-check (check-equal?/any equal-with-anys? actual expected))

(define (test desired p . args)
    (let* ([result (apply p args)])
      (unless (equal-with-anys? desired result)
        (fprintf (current-error-port) "test failed: desired: ~v\ngot: ~v\ntest: ~v\n" desired result (cons p args)))))

;; check a and b for equality, except that 'any 
;; matches anything. Poor man's pattern system.
(define (equal-with-anys? a b)
    (cond [(eq? a 'any) #t]
          [(eq? b 'any) #t]
          [(and (cons? a) (cons? b))
           (and (equal-with-anys? (car a) (car b))
                (equal-with-anys? (cdr a) (cdr b)))]
          [else (equal? a b)]))


;; SPOTTY TESTING

(define (check-build-arity-table expected stx)
  (check-equal?/any
   (map (lambda (key-value-list)
          (list (syntax-e (car key-value-list))
                (cadr key-value-list)))
        (build-arity-table 
         (expand (datum->syntax #'here stx))))
   expected))

(check-build-arity-table `((a ((2 2)))) '(define a (lambda (b c) b)))
(check-build-arity-table `((a ((1 1)))) '(begin (define (a x) 3)))
(check-build-arity-table `((a ((3 3) (2 inf)))) '(define a (case-lambda ((a b c) 3) ((a b . c) 3))))
(check-build-arity-table `() '(define a (if #t (lambda (b c) 3) (lambda (c) 3))))
(check-build-arity-table `((a ((2 2))))
                        #'((let*-values ([(a) (lambda (b c) 3)]
                                         [(b c) (values (lambda (b) 3) (lambda (x) 3))]
                                         [(d) (begin (lambda (a b) 3) (lambda (a) 3))])
                             (set! d (lambda (a b c d e) 3)))))
(check-build-arity-table `((a ((1 1))) (a ((2 2))))
                        #'((let ([a (lambda (x) x)])  3)
                           (let ([a (lambda (x y) x)]) 3)))
(check-build-arity-table `((a ((1 1))))
                        '(define a (begin (lambda () 3) (begin0 (lambda (x) 3) (lambda () 3)))))
(check-build-arity-table `((a ((1 1))) (b ((1 1))))
                        #'(+ (begin (let ([a (lambda (x) x)]) 3) 4) 
                             (begin0 4 (let ([b (lambda (x) x)]) 3))))
(check-build-arity-table `()
                        '(define (a x) (set! a (lambda (x y) 3))))

(check-build-arity-table
 `((f ((1 1))))
 `(module test-module scheme
    (define (f x) 6)    
    (f 4)))


;; check-program testing:



;; not bad testing

(define a-id (expand #'a))
(define a-id-stripped (syntax-case a-id (#%top)
                        [(#%top . a)
                         #'a]))
(define b-id (expand #'b))
(define b-id-stripped (syntax-case b-id (#%top)
                        [(#%top . b)
                         #'b]))
(define c-id (expand #'c))
(define c-id-stripped (syntax-case c-id (#%top)
                        [(#%top . c)
                         #'c]))
(define id-list (list a-id-stripped b-id-stripped c-id-stripped))
(define arities-list `(((2 2)) ((1 3) (5 inf)) ((3 3))))

(define arity-table 
  (map list id-list arities-list))

(define-syntax (check-program-test stx)
  (syntax-case stx ()
    [(_ expected in-stx)
     (syntax/loc stx 
       (check-equal?/any
        (make-testable
         (check-program (expand (datum->syntax #'here in-stx)) arity-table))
        expected))]))

(define-syntax (check-shebang stx)
  (syntax-case stx ()
    [(_ expected in-stx)
     (syntax/loc stx
       (check-equal?/any
        (make-testable
         (let ([expanded (expand (datum->syntax #'here in-stx))])
           (check-program expanded 
                          (build-arity-table expanded))))
        expected))]))

(define (make-testable result)
  (map 
   (lambda (result)
     (match result
       [(list-rest a b c-rest)
        (cons a 
              (cons (syntax->datum b) 
                    (map (lambda (_) (if (pair? _)
                                         (map (lambda (_) (if (syntax? _)
                                                              (syntax->datum _)
                                                              _))
                                              _)
                                         _))
                         c-rest)))]))
   result))

(define d1 `(quote 1))
(define d2 `(quote 2))
(define d3 `(quote 3))
(define d4 `(quote 4))
(define d5 `(quote 5))
(define ta `(#%top . a))
(define tb `(#%top . b))
(define tc `(#%top . c))

(check-program-test `((application-ok (#%app ,ta ,d3 ,d4))) `(,a-id 3 4))
(check-program-test `((bad-application (#%app ,ta ,d3) ((2 2)))) `(,a-id 3))
(check-program-test `((unknown-id-application (#%app (#%top . f) ,d3))) `(f 3))
(check-program-test `((unknown-id-application (#%app + x ,d1))
                      (non-id-application (#%app (lambda (x) (#%app + x ,d1)) ,d3))) 
                    `((lambda (x) (+ x 1)) 3))
(check-program-test `((application-ok (#%app ,tc ,d1 ,d2 ,d3)) 
                      (bad-application (#%app ,tc ,d1 ,d2) ((3 3)))
                      (non-id-application (#%app (#%app ,tc ,d1 ,d2) ,d3))
                      (application-ok (#%app ,tb ,d1 ,d2 ,d3 ,d4 ,d5))) 
                    `(if (,b-id 1 2 3 4 5) ((,c-id 1 2) 3) (,c-id 1 2 3)))
(check-program-test `() `(begin 3 4))
(let* ([stx (expand (datum->syntax #'here `(module foo scheme (define (h x) (h x)))))]
       [id (syntax-case stx ()
             [(mod dc1 dc2 (mod-beg mcr (define-values (id) . dc3)))
              #'id])]
       [table `((,id ((1 1))))])
  (test `((application-ok (#%app h x))) 
        make-testable (check-program stx table)))

(check-program-test `((unused-bindings any lambda (x)))
                    `(if 3 (lambda (x) 4) 8))

(check-program-test `((unknown-id-application any)
                      (unused-bindings any let/rec (z)))
                    `(lambda (y) (let ([z 3] [q y]) (+ q y))))


(check-program-test `((unused-bindings any module (a b)))
                    `(module foo scheme
                       (provide (except-out (all-defined-out) a b c d)
                                c)
                       (define z 1)
                       (define b 13)
                       (define a d)
                       (define c 287)
                       (define d 9)
                       (define q z)))

#;`(module test-module scheme
     
     (define (f x) 6)
     
     (f 4))

(check-shebang
 `((application-ok (#%app any ,d4))
   (unknown-id-application (#%app call-with-values any any)))
 `(module test-module scheme
    
    (define (f x) x)
    
    (f 4)))


(check-shebang
 `((bad-application (#%app f ,d3 ,d4 ,d5) ((2 2)))
   (unknown-id-application (#%app call-with-values any any))
   (unknown-id-application (#%app + any any))
   (unused-bindings (let-values any any) let/rec (z))
   (unused-bindings (lambda (x y) any) lambda (y))
   (unused-bindings (module test-module scheme any) module (a)))
 `(module test-module scheme
    
    (define a 3)
    
    (define (f x y) (let ([z 34])
                      (+ x 3)))
    
    (f 3 4 5)))

;; 2013: I have no idea what this test was supposed to do.
;; doesn't work correctly with keyword arguments...
#;(check-shebang
 `((bad-application (#%app foo ))))
 