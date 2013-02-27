(module arity-table mzscheme
  
  (require (lib "contract.ss")
           (lib "list.ss"))
  
  (define limit?
    (union number? (symbols 'inf)))
  
  (define limit/unknown?
    (union limit? (symbols 'unknown)))
  
  (define table?
    (listof (list/c identifier? (listof (list/c limit? limit?)))))
  
  (define table-with-unknown?
    (listof (list/c identifier? (listof (union (symbols 'unknown) (list/c limit? limit?))))))
  
  (provide/contract
   [limit? contract?]
   [limit/unknown? contract?]
   [table? contract?]
   [table-with-unknown? contract?]
   [coalesce-table (-> table-with-unknown? table?)]
   [find-match (-> identifier? table? (union false/c (list/c identifier? (listof (list/c limit? limit?)))))]
   [arity-match (-> (listof (list/c limit? limit?)) number? boolean?)])
  
  (define (coalesce-table list-table) 
    (let* ([coalesced (let loop ([list-table list-table])
                           (if (null? list-table)
                               null
                               (let* ([matching-elts (apply append (map cadr (filter (lambda (table-elt)
                                                                                       (module-identifier=? (car table-elt) 
                                                                                                            (caar list-table)))
                                                                                     list-table)))]
                                      [rest (loop (filter (lambda (table-elt)
                                                            (not (module-identifier=? (car table-elt)
                                                                                      (caar list-table))))
                                                          (cdr list-table)))])
                                 (if (memq 'unknown matching-elts)
                                     rest
                                     (cons (list (caar list-table) matching-elts)
                                           rest)))))])
         coalesced))
  
    
  (define (find-match identifier table)
    (ormap (lambda (entry) 
             (if (module-identifier=? (car entry) identifier)
                 entry
                 #f)) 
           table))
  
  (define (arity-match arity-list numargs)
    (ormap (lambda (interval)
             (and (>= numargs (car interval))
                  (or (eq? (cadr interval) 'inf)
                      (<= numargs (cadr interval)))))
           arity-list)))
  
