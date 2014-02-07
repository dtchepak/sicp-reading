
; 2.17
(define (foldr f e l)
  (if (null? l)
    e
    (f (car l) (foldr f e (cdr l)))))
(define (foldl f e l)
  (if (null? l)
    e
    (foldl f (f e (car l)) (cdr l))))

(define (flip f)
  (lambda (b a) (f a b)))

(define nil '())

(define (reverse a)
  (foldl (flip cons) nil a))
