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
; Procedural abstraction 
; - a caller should not know how a procedure is implemented to be 
;   able to use it. (proc is black box)
; - to keep black box, parameter names must be local to proc body
;   * a proc *binds* its formal params - bound variable.
;   * unbound variable is *free*.
;   * set of expressions for which binding defines name is *scope* of name
;   * meaning of a proc is independent from names of its bound variables
;     (not from free variables; which depend on external scope)
;
; Block structure
; - internal definitions to proc
; - get access to bound variables of outer scope (*lexical scoping*)

;;Lecture 1B
; linear: O(n)
; linearly recursive (space O(n), time O(n)):
(define (fib n)
  (if (< n 2) n
      (+ (fib (-1+ n)) (fib (- n 2)))))
; linearly iterative (space O(1), time O(n)):
(define (fibi n)
  (define (fib-iter count last2 last1)
    (if (= count n) last1
        (fib-iter (1+ count) last1 (+ last2 last1))))
  (fib-iter 0 1 0))

; towers of hanoi
; TODO

;1.2 Procedures and processes
; recursive process: 
; - expansion followed by contraction (in space)
; - characterised by chain of deferred operations.
; - have to keep track of operations yet to be performed
; - linear recursive: space grows proportionally to n
; - maintains "hidden" info on "where the process is". can't 
;   stop then resume computation as part of each step is deferred
; iterative process: 
; - constant space
; - keeps track of state at each step; rules describe transitions
; - linear iterative: number of steps (time) grows proportionally to n
; - if we stop computation midway, could resume with current state.
;   (no additional "hidden" information about the state of the comp)
; recursive process vs. recursive procedure
; - recursive procedure: syntactic fact that proc refers to itself
; - recursive process: how the process evolves; not syntax based

;;Exercise 1.9:
; 1) (define (+ a b) (if (= a 0) b (inc (+ (dec a) b))))
; 2) (define (+ a b) (if (= a 0) b (+ (dec a) (inc b))))
; For 1:
; (+ 4 5)
; = (if (= 4 0) 5 (inc (+ (dec 4) 5)))
; = (if #f 5 (inc (+ 3 5)))
; = (inc (+ 3 5))
; = (inc (inc (+ 2 5))
; = (inc (inc (inc (+ 1 5))))
; = (inc (inc (inc (inc (+ 0 5)))))
; = (inc (inc (inc (inc 5))))
; = (inc (inc (inc 6)))
; = (inc (inc 7))
; = (inc 8)
; = 9
; Space grows with a, so linearly recursive. (With each incr in a, 
; one extra deferred comp has to be maintained)
; For 2:
; (+ 4 5)
; = (if (= 4 0) 5 (+ (dec 4) (inc 5)))
; = (+ (dec 4) (inc 5))
; = (+ 3 6)
; = (+ 2 7)
; = (+ 1 8)
; = (+ 0 9)
; = 9
; Space is constant, so linearly iterative. (Each incr in a adds an
; extra step, but no extra state between steps)

;;Exercise 1.10:
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))
(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))
; (f n) = (A 0 n)
;       = (* 2 y)
; So f(n) = 2y
; 
; (g 0) = 0
; (g 1) = 2
; (g n) = (A 0 (A 1 (- n 1)))
;       = (f (g (- n 1)))
;       = (* 2 (g (- n 1)))
; So g(n) = 2g(n-1) for n >= 2
; g(3) = 2g(2)
;      = 2(2(g 1))
;      = 2*2*2 = 2^3
; So g(n) = 2^n if n>0, else 0
;
; (h 0) = (A 2 0) = 0
; (h 1) = (A 2 1) = 2
; (h n)
;   = (A 2 n)
;   = (A 1 (A 2 (- n 1)))
;   = g (A 2 (- n 1))
;   = g (h (- n 1))
; If (n-1) >1
;   = g (g (h (- n 2))) 
; (h 2) = g (h 1)
;       = g (2)
;       = 2^2
; (h 3) = 2^2^2
; So (h n) = 2^2^...^2 (n times)

; k(n) = 5n^2

; (A 1 10) = g(10) = 2^10 = 1024
; By substitution model:
; = (A (- 1 1) (A 1 (- 10 1)))
; = (A 0 (A 1 9))
; = (A 0 (A 0 (A 1 8)))
; = (A 0 (A 0 (A 0 (A 1 7))))
; = ...
; = (A 0 (A 0 (... (A 1 1))))
; = (A 0 (A 0 (... (A 0 2)))

; (A 2 4) = h(4) = 2^2^2^2 = 65536
; (A 3 3) = A 2 (A 3 2)
;         = h (A 3 2)
;         = h (A 2 (A 3 1))
;         = h (h 2)
;         = h (2^2)
;         = h 4
;         = 65536 (from (A 2 4))

;1.2.2 Tree recursion
; e.g. fib
; Steps grow exponentially
; Space grows linearly

;;Exercise 1.11

(define (ex1.11r n)
    (if (< n 3) n
        (+ (ex1.11r (- n 1))
           (* 2 (ex1.11r (- n 2)))
           (* 3 (ex1.11r (- n 3))))))
; (ex1.11r 3)
; = (+ (ex1.11r 2) (* 2 (ex1.11r 1)) (* 3 (ex1.11r 0)))
; = (+ 2 2 0)
; = 4

; f(0) = 0
; f(1) = 1
; f(2) = 2
; f(3) = f(2) + 2f(1) + 3f(0)
; f(4) = f(3) + 2f(2) + 3f(1)

(define (ex1.11i n) 
    (define (next a b c) (+ a (* 2 b) (* 3 c)))
    (define (step count x y z)
        (cond
            ((= count n) (next x y z))
            (else (step (+ count 1) (next x y z) x y))))
    (if (< n 3) n
        (step 3 2 1 0)))

(define (ex1.11r n)
    (cond
        ((< n 3) n)
        (else (+ (ex1.11r (- n 1))
                 (* 2 (ex1.11r (- n 2)))
                 (* 3 (ex1.11r (- n 3)))))))

;;Exercise 1.12
; Pascal's triangle
;    1
;   1 1
;  1 2 1
; 1 3 3 1
; .......
(define (pascal row col)
    (define inTriangle?
        (and (>= row 1) (>= col 1) (<= col row)))
    (define edge?
        (or (= col 1) (= col row)))
    (cond
        ((not inTriangle?) 0)
        (edge? 1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))


;;Exercise 1.13
; http://www.billthelizard.com/2009/12/sicp-exercise-113-fibonacci-and-golden.html

;;Exercise 1.14
; (count-change 11)
;                                  (cc 11 5)
;                          (cc 11 4)                     (0: cc -39 5)
;                  (cc 11 3)        (0: cc -14 5))
;       (cc 11 2)           (cc 1 3)
;     (cc 11 1)  (cc 6 2) (cc 1 2)  (0: cc -9 3)
;(0:cc 11 0)  
; [TODO: come back to this later]


;;Exercise 1.15
(define (sine angle)
  (define (cube x) (* x x x))
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

; (sine 12.15)
; = (p (sine 4.05))
; = (p (p (sine 1.34999)))
; = (p (p (p (sine 0.4499))))
; = (p (p (p (p (sine .15)))))
; = (p (p (p (p (p (sine .0499))))))
;
; 1.15 a) p is applied 5 times
; 1.15 b) Number of steps and space depends on how quickly angle gets 
;         <= 0.1. If we need to divide by angle n times:
;
;         angle/3^n <= 0.1
;         angle <= 0.1 . 3^n
;         3^n <= angle/0.1
;         3^n <= 10 . angle
;
;         Log defn: x^n = y
;                   logx(y) = n
;
;         n <= log3(10 . angle)
;         n <= log3(10) + log3(angle)
;
;         So:
;         O(sine(x)) = O(log(x))
;
;         (Change base: logb(x) = logk(x) / logk(b))
;         (Natural log: logb(x) = log(x) / log(b))
;
;         Scott's note: Can change algorithm to take into account
;         angle is [0, 2π], so upper bound can be O(log(2π)), which
;         means a constant upper bound O(1).

;1.2.4 Exponentiation
;=====================
;;Exercise 1.16
; Given:
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;  b^n = (b^n/2)^2 if n is even
;  b^n =b·b^(n-1) if n is odd.
(define (fast-expt-i b n)
  (define (step a bb nn)
    (cond ((= nn 0) a)
          ((odd? nn) (step (* a bb) bb (- nn 1)))
          (else (step a (square bb) (/ nn 2)))))
  (step 1 b n))

;;Exercise 1.17
;; if b is even:  * a b = * 2 a (/ b 2) 
;; * 3 3 
;; = + 3 (* 3 2)
;; = + 3 (+ 3 (* 3 1))
;; = + 3 (+ 3 (+ 3 (* 3 0)))
;; Alt:
;; = + 3 (* 3 2)
;; = + 3 (* 6 1)
;; = + 3 (+ 6 (* 6 0))
;; * 3 4
;; = (* 6 2)
;; = (* 12 1)
;; mult 2 6
;; = step 0 2 6
;; = step 0 4 3
;; = step 4 4 2
;; = step 4 8 1
;; = step 12 8 0
(define (mult a b)
  (define (double x) (* 2 x))
  (define (halve x) (/ x 2))
  (define (step x aa bb)
    (cond ((= bb 0) x)
          ((odd? bb) (step (+ x aa) aa (- bb 1)))
          (else (step x (double aa) (halve bb)))))
  (step 0 a b))

;; If b doubles, takes one additional step -> O(log b)

;;Exercise 1.18
;; As per 1.17

;;Exercise 1.19
; Tpq (a,b)
;   = (bq + aq + ap, bp + aq)
; Tpq (Tpq (a,b))
;   = ( (bp+aq)q + (bq+aq+ap)q + (bq+aq+ap)p
;     , (bp+aq)p + (bq+aq+ap)q)
;   = ( bpq + aq² + bq² + aq² + apq + bpq + apq + ap²
;     , bp² + apq + bq² + aq² + apq)
;   = ( 2bpq + 2aq² + 2apq + bq² + ap²
;     , bp² + 2apq + bq² + aq²)
;
; Let x=p', y=q' cause its a bit easier to type
; Txy (a,b) = (by + ay + ax, bx + ay)
;           = Tpq (Tpq (a,b))
; Take fst:
; by + ay + ax
;   = 2bpq + 2aq² + 2apq + bq² + ap²
;   = b(2pq+q²) + 2aq² + 2apq + ap²
;   = b(2pq+q²) + aq² + aq² + 2apq + ap²
;   = b(2pq+q²) + a(2pq+q²) + a(p²+q²)
; So y=2pq+q², x=p²+q²
; Check snd:
; bx + ay
;   = bp² + 2apq + bq² + aq²    .... (1)
; Sub for x and y:
;   = b(p²+q²)+a(2pq+q²)
;   = bp²+bq²+2apq+aq²
;   = (1) as required
(define (fibagain n)
   (define (fib-iter a b p q count)
     (cond ((= count 0) b)
           ((even? count)
            (fib-iter a
                      b
                      (+ (square p) (square q))
                      (+ (* 2 p q) (square q))
                      (/ count 2)))
           (else (fib-iter (+ (* b q) (* a q) (* a p))
                           (+ (* b p) (* a q))
                           p
                           q
                           (- count 1)))))
   (fib-iter 1 0 0 1 n))

;1.2.5 Greatest Common Divisors
; Euclid's algorithm:
; GCD(a,0) = a
; GCD(a,b) = GCD(b,r)
;   where r = a % b
; from text example:
(define (gcd a b)
      (if (= b 0)
          a
          (gcd b (remainder a b))))
;; Exercise 1.20
;
; GCD(206, 40) normal order eval:
; (gcd 206 40)
; = (if (= 40 0) 206 (gcd 40 (r 206 40)))
; = (gcd 40 (r 206 40))
; = (if (= (r 206 40) 0) 40 (gcd (r 206 40) (r 40 (r 206 40))))
; >> 1 r eval for the if predicate
; = (gcd (r 206 40) (r 40 (r 206 40)))
; = (if (= (r 40 (r 206 40)) 0)
;       (r 206 40)
;       (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
; >> 2 r evals for the if preciate
; = (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
; >> 4 r evals from b for if predicate
; = (gcd (r (r 206 40) (r 40 (r 206 40)))
;        (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
; >> 7 r evals, now b=0
; = (r (r 206 40) (r 40 (r 206 40)))
; >> 4 evals
; >> Total 1+2+4+7+4 = 18

; GCD(206, 40) applicative order eval:
; (gcd 206 40)
; = (if (= 40 0) 206 (gcd 40 (r 206 40)))
; >> 1 r eval for second arg of recursive gcd call
; = (gcd 40 6) 
; >> 1 eval
; = (gcd 6 4)
; >> 1 eval
; = (gcd 4 2)
; >> 1 eval
; = (gcd 2 0)
; = 2 
; >> Total 4


;;Exercise 1.21
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

;1 ]=> (smallest-divisor 199)
;Value: 199
;1 ]=> (smallest-divisor 1999)
;Value: 1999
;1 ]=> (smallest-divisor 19999)
;Value: 7

;;Exercise 1.22
;;Exercise 1.23
;;Exercise 1.24
; Skipping timing exercises

;;Exercise 1.25
;from SICP:
(define (expmod base exp m)
     (cond ((= exp 0)
            1)
           ((even? exp)
            (remainder
             (square
              (expmod base (/ exp 2) m))
             m))
           (else
            (remainder
             (* base
                (expmod base (- exp 1) m))
             m))))
; compare:
;  (define (expmod base exp m)
;      (remainder (fast-expt base exp) m))
;  (define (fast-expt b n)
;      (cond ((= n 0) 1)
;        ((even? n) (square (fast-expt b (/ n 2))))
;        (else (* b (fast-expt b (- n 1))))))
; fast-expt works out full exp, then takes remainder.
; expmod takes mod at each step so never deals with huge numbers.
;
; http://www.billthelizard.com/2010/02/sicp-exercise-125-closer-look-at-expmod.html

;;Exercise 1.26
; Was O(log n), now each iteration produces another 2 calcs. Tree recurive
; So get O(log (2^n)) = O(n)

;Exercise 1.27
; skipped

;Exercise 1.28
; skipped



