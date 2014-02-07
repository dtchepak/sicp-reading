(define (square a) (* a a))

(define (sum-of-squares a b) (+ (square a) (square b)))

(define (smallest? x y z) (and (< x y) (< x z)))

(define (f a b c)
  (cond ((smallest? a b c) (sum-of-squares b c))
        ((smallest? b a c) (sum-of-squares a c))
        (else (sum-of-squares a b))
        ))

(define (sqrt-iter lastGuess guess x)
  (if (good-enough? lastGuess guess)
    guess
    (sqrt-iter guess (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? lastGuess guess)
  (< (abs (- guess lastGuess)) 0.0001))

(define (sqrt x)
  (sqrt-iter 0.0 1.0 x))

; 1.8

(define (cube-iter last guess x)
  (define (good-enough? g l)
    (< (abs (- g l)) 0.00001))
  (define (improve g)
     (/ (+ 
          (/ x (* g g)) 
          (* 2.0 g)) 
        3.0))
  (if (good-enough? guess last)
    guess
    (cube-iter guess (improve guess) x)))

(define (cubert x) (cube-iter 0.01 1.0 x))
