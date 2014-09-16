; repl helpers
(define (abort) (restart 1))
(define (l x) (load x))
(define (reload) (load "chapter2.scm"))

; 2 Building Abstractions with Data
; ===================================
; Procedures define local evolution of process
; Compound procedures let us move from local evolution to
; thinking more abstractly, enhancing our language to describe
; the problem in abstract terms.

; Compound data lets us do the same thing, elevating conceptual
; level above primitive data objects.
; Compound data increase modularity - we can separate how data
; object is represented from how they are used. => DATA ABSTRACTION


