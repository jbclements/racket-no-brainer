(module no-brainer-vc mzscheme
  (require (lib "unit.ss")
           "../no-brainer-sig.ss"
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "framework.ss" "framework"))
  
  (provide no-brainer-vc@)
  
  (define-unit no-brainer-vc@ 
    
    (import drs-window^)
    (export no-brainer-vc^)
    
    (define (receive-string result)
      (send-output-to-debugger-window result debugger-output))
    
    
    (define debugger-output (make-output-window drs-window)))
  
  ;; Info functions:
  
  ;; Debugger Output Window:
  
    
  ; make-output-window : (-> text:basic%)
  (define (make-output-window drs-window)
    (let* ([frame (instantiate frame:basic% () 
                    (label "Things You Might Want To Fix")
                    (width 400)
                    (height 400))]
           [canvas (instantiate canvas:basic% () (parent (send frame get-area-container)))]
           [text (instantiate text:basic% ())])
      (send canvas set-editor text)
      (send frame show #t)
      text))
  
  ; send-output-to-debugger-window : (string text:basic% -> void)
  (define (send-output-to-debugger-window str text)
    (send text insert str (send text last-position))))      
      
    