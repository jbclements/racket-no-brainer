(module no-brainer racket/base
  (require racket/unit
           "../no-brainer-sig.rkt"
           racket/match
           "build-arity-table.rkt"
           "arity-table.rkt"
           "check-program.rkt")
  
  (provide no-brainer@)
  
  (define-unit no-brainer@
    
    (import no-brainer-vc^ expander^)
    (export no-brainer^)
    
    (define (my-error-display-handler msg exn)
      (receive-string
       (format "ERROR DURING EXPANSION:\n  ~a\n" msg)))
    
    (define (go)
      (parameterize ([error-display-handler my-error-display-handler])
        (program-expander expr-iterator)))
    
    (define table null)
    (define results null)
    
    
    ; header, predicate & printer sets:
    
    ; bad application
    
    (define bad-app-header "\nAPPLICATIONS WITH WRONG ARITY\n\n")
    
    (define (bad-app-predicate result)
      (eq? (car result) 'bad-application))
    
    (define (bad-app-printer result)
      (receive-string
       (format "mis-application at ~a with desired arities: ~a\n" 
               (list-ref result 1)
               (list-ref result 2))))
    
    ; unused let/rec bindings
    
    (define unused-bindings-header "\nUNUSED LET/REC BINDINGS\n\n")
    
    (define (unused-bindings-predicate result)
      (and (eq? (car result) 'unused-bindings)
           (eq? (list-ref result 2) 'let/rec)
           (ormap syntax-position (list-ref result 3))))
    
    (define (unused-bindings-printer result)
      (receive-string
       (format "bindings ~a (from ~a) unused in expression: ~a\n"
               (map syntax-e (list-ref result 3))
               (list-ref result 3)
               (list-ref result 1))))
    
    ; unused module definitions
    
    (define unused-module-defs-header "\nUNUSED DEFINITIONS IN MODULE\n\n")
    
    (define (unused-module-defs-predicate result)
      (match result
        [`(unused-bindings ,stx module ,bindings)
          (ormap syntax-position bindings)]
        [else
         #f]))
    
    (define (unused-module-defs-printer result)
      (receive-string
       (apply string-append
              (map (λ (_) (format "defined value ~a from ~a unused in module\n"
                               (syntax->datum _)
                               _))
                   (list-ref result 3)))))
    
    ; unused lambda bindings
    
    (define unused-lam-bindings-header "\nUNUSED LAMBDA BINDINGS\n\n")
    
    (define (unused-lam-bindings-predicate result)
      (and (eq? (car result) 'unused-bindings)
           (eq? (list-ref result 2) 'lambda)
           (ormap syntax-position (list-ref result 3))))
    
    (define (unused-lam-bindings-printer result)
      (receive-string
       (format "bindings ~a (from ~a) unused in expression: ~a\n"
               (map syntax-e (list-ref result 3))
               (list-ref result 3)
               (list-ref result 1))))
    
    
    (define (loop-and-print header predicate printer results)
      (receive-string header)
      (let loop ([remaining results])
        (unless (null? remaining)
          (when (predicate (car remaining))
            (printer (car remaining)))
          (loop (cdr remaining)))))
    
    
    (define (expr-iterator expr recur)
      (if (eof-object? expr)
          (begin
            (loop-and-print bad-app-header bad-app-predicate bad-app-printer results)
            (loop-and-print unused-bindings-header unused-bindings-predicate unused-bindings-printer results)
            (loop-and-print unused-module-defs-header unused-module-defs-predicate unused-module-defs-printer results)
            (loop-and-print unused-lam-bindings-header unused-lam-bindings-predicate unused-lam-bindings-printer results))
          (begin
            (set! table (coalesce-table (append (build-arity-table expr) table)))
            (set! results (append (check-program expr table) results))
            (recur))))))
