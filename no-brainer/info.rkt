(module info setup/infotab
  (define name "No-Brainer")
  (define blurb `("A simple static checker that reports some local arity errors and unused bindings"))
  (define release-notes
    `("This version uses planet2."))
  (define categories `(devtools))
  (define drracket-tools '(("no-brainer-tool.rkt")))
  (define drracket-tool-names (list "Simple Static Checker"))
  (define compile-omit-paths '("private/test.rkt"))
  (define scribblings '(("no-brainer.scrbl" ())))
  (define version "20130227")
  )
