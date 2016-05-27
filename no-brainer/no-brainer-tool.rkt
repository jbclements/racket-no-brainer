#lang racket/base
  
(require racket/contract
         drracket/tool
         racket/gui/base
         (prefix-in frame: framework)
         mrlib/switchable-button
         racket/class
         racket/list
         racket/unit
         racket/runtime-path
         "no-brainer-sig.rkt"
         "private/no-brainer-vc.rkt"
         "private/no-brainer.rkt")
  
(define-runtime-path icon-path "icon.png")

  (provide tool@)
  
  (define-unit tool@ 
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    (define (phase1) (void))
    (define (phase2) (void))
    
    (define debugger-initial-width 500)
    (define debugger-initial-height 500)
    
    
    (define (debugger-unit-frame-mixin super%)
      (class* super% ()
        
        (inherit get-button-panel register-toolbar-button get-interactions-text get-definitions-text)
        
        (super-instantiate ())
        
        (define program-expander
          (contract
           (-> (-> (or/c eof-object? syntax? (cons/c string? any/c)) (-> any) any) ; iter
               void?)
           (lambda (iter)
             (let* ([lang-settings 
                     (frame:preferences:get
                      (drracket:language-configuration:get-settings-preferences-symbol))])
               (drracket:eval:expand-program
                (drracket:language:make-text/pos (get-definitions-text)
                                                 0
                                                 (send (get-definitions-text)
                                                       last-position)) 
                lang-settings
                #t
                ; set current-directory and current-load-relative-directory before expansion
                (lambda ()
                  (let* ([tmp-b (box #f)]
                         [fn (send (get-definitions-text) get-filename tmp-b)])
                    (unless (unbox tmp-b)
                      (when fn
                        (let-values ([(base name dir?) (split-path fn)])
                          (current-directory base)
                          (current-load-relative-directory base))))))
                void ; kill
                iter)))
           'program-expander
           'caller))

        (define no-brainer-bitmap (make-object bitmap% icon-path 'png/mask))
        
        (define no-brainer-button
          (new switchable-button% 
               [parent (get-button-panel)]
               [label "-Wall"]
               [bitmap no-brainer-bitmap]
               [alternate-bitmap no-brainer-bitmap]
               [callback (lambda (button)
                           (start-analysis program-expander this))]))

        (register-toolbar-button no-brainer-button)

        (define-compound-unit/infer froogy@
          (import drs-window^ expander^)
          (export no-brainer^)
          (link no-brainer@ no-brainer-vc@))

        (define (start-analysis program-expander drs-window)
          (define-values/invoke-unit/infer froogy@)
          (go))

        (define/augment (enable-evaluation)
          (send no-brainer-button enable #t)
          (inner (void) enable-evaluation))
        
        (define/augment (disable-evaluation)
          (send no-brainer-button enable #f)
          (inner (void) disable-evaluation))
        
        (send (get-button-panel) change-children
              (λ (_) (cons no-brainer-button (remq no-brainer-button _))))))
    
    (drracket:get/extend:extend-unit-frame debugger-unit-frame-mixin))
