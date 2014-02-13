; repl helpers
(define (abort) (restart 1))
(define (l x) (load x))
(define (reload) (load "chapter1.scm"))

;1.1.4
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
;   - left, innermost
; Normal order:
;   - "fully expand and then reduce"
;   - substitute operand expressions for params until
;     expr contains only primitives, then evaluate.
;   - left, outermost
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

;;Exercise 1.4
; If b < 0, then the following evaluates to '-':
;   (if (> b 0) + -)
; In this case (a-plus-abs-b a b) evaluates to:
;   (- a b)
; Similarly for b >= 0, the operator applied to 'a b' will be +

;;Exercise 1.5
; For applicative order, operator and operand expressions will be evaluated 
; first.
; In (test 0 (p)), (p) is defined as (p), so this will evaluate to
; (test 0 (p)). This evaluation will continue indefinitely, so the program
; will not terminate.
; 
; For normal order, each expression will be replaced with its definition.
; (test 0 (p))
; => (if (= 0 0) 0 (p))     ... defn of test, x = 0
; => (if #t 0 (p))          ... eval =
; => 0                      ... defn of if

;;Exercise 1.6
; With applicative order, parameters given to new-if will be evaluated first. As sqrt-iter is defined recursively, it will continue to evaluate indefinitely without short-circuiting when the predicate becomes true.

;;Exercise 1.7
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average a b)
  (/ (+ a b) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x) (sqrt-iter 1.0 x))

; (good-enough? 0.03 0.0001) => #t, when (sqrt 0.0001) = 0.01
; So the approximation is out by a factor of 3.
; For large x, the guesses are never good enough.
; For example, sqrt of 1e100 is 1e50. But (square 1e50) is evaluated as
; 1.0000000000000002e100, which is > 0.001 difference due to truncation
; of less significant bits. If values within that tolerance can not be 
; represented in terms of square then the alg will never terminate. (?)

(define (good-enough?? last-guess this-guess)
  (< (abs (- last-guess this-guess)) 0.001))
(define (sqrt-iter2 last-guess this-guess x)
  (if (good-enough?? last-guess this-guess)
    this-guess
    (sqrt-iter2 this-guess (improve this-guess x) x)))
(define (sqrt2 x) (sqrt-iter2 0 1.0 x))

; This passes both 1e100 and 0.0001 tests.

;;Exercise 1.8
(define (cube-root x)
  (define (improve y)
    (/ (+
         (/ x (square y))
         (* 2 y))
       3))
  (define (cube-root-iter last-guess this-guess)
    (if (good-enough?? last-guess this-guess)
      this-guess
      (cube-root-iter this-guess (improve this-guess))))
  (cube-root-iter 0 1.0))

;1.1.8 Procedures as black-box abstractions
;
