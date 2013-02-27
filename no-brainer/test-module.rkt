(module test-module mzscheme
  
  (define a 3)
  
  (define (f x y) (let ([z 34])
                    (+ x 3)))
  
  (f 3 4 5))
