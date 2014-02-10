; repl helpers
(define (abort) (restart 1))
(define (l x) (load x))
(define (reload) (load "chapter1.scm"))

; 1.4
(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(f 5)

;1.1.5
; Substitution model
; ==================
; Evaluate combination:
; - replace proc name with body
; - replace each formal parameter with coresponding arg
; Example:
; (f 5)
; f = ((sum-of-squares (+ a 1) (* a 2)) 5)
; a = 5
; (f 5) = (sum-of-squares (+ 5 1) (* 5 2))
; => (sum-of-squares 6 10)
; => (+ (square 6) (square 10))
;

; Applicative order vs. normal order
; ==================================
; Applicative order: 
;   - "evaluate the arguments then apply"
;   - eval operator and operands, then apply
; Normal order:
;   - "fully expand and then reduce"
;   - substitute operand expressions for params until
;     expr contains on ly primitives, then evaluate.
; These are equivalent for procs that yield legitimate values
;
; parameter: place-holder for arg to a procedure
; argument: expr passed as for parameter 
;
; Normal order:
;    (sum-of-squares (+ 5 1) (* 5 2))
; -> (+ (square (+ 5 1)      (square (* 5 2)))  )
; -> (+ (* (+ 5 1) (+ 5 1))  (* (+ 5 1) (+ 5 1)))
;

;1.1.6
; cond, if and other special forms
; only evaluate the sub-expressions required to reduce
(define (abs2 x) 
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs3 x) 
  (cond ((< x 0) (- x))
        (else x)))

; (if <predicate> <consquent> <alternative>)
; (and <e1> ... <en>)
; (or <e1> ... <en>)
; (not <e>)

;;Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;;Exercise 1.3
(define (ex1.3 a b c)
  (define (apply-to-top-2 f a b c)
    (cond ((and (< a b) (< a c)) (f b c))
          ((and (< b a) (< b c)) (f a c))
          (else (f a b))))
  (apply-to-top-2 sum-of-squares a b c))

