(module no-brainer-sig mzscheme
  (require (lib "unit.ss"))
  
  (provide no-brainer^
           no-brainer-vc^
           expander^
           drs-window^)
  
  (define-signature no-brainer^
    (go))
  
  (define-signature no-brainer-vc^
    (receive-string))
  
  (define-signature expander^ (program-expander))
  
  (define-signature drs-window^ (drs-window)))
