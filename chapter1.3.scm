; 1.42
(define (double f) (compose f f))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeated-iter f n r)
    (if (= 0 n)
      r
      (repeated-iter f (- n 1) (f r))))
  (lambda (x) (repeated-iter f n x)))

(define (repeated2 f n)
  (if (= 1 n)
    f
    (compose f (repeated2 f (- n 1)))))

(define (id x) x)

(define (repeated3 f n)
  (if (< n 1) 
    id
    (compose f (repeated3 f (- n 1)))))
