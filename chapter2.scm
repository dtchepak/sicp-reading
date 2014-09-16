; repl helpers
(define (abort) (restart 1))
(define (l x) (load x))
(define (reload) (load "chapter2.scm"))

