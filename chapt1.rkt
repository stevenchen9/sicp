(define size 2)

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(define circumference (* 2 pi radius))
(define (square x) (* x x))
(square 41)
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (-x))))
(define (abs2 x)
  (cond ((< x 0) (-x))
        (else x)))
(define (abs3 x)
  (if (< x 0)
      (-x)
      x))
(define (>= x y)
  (not (< x y)))

;; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
   (* 3 (- 6 2) (- 2 7)))
;; 1.3
(define (sq x) (* x x))
(define (sum-of-squares2 x y z)
  (+ (sq x) (sq y) (sq z)))
(sum-of-squares2 1 2 3)
;; 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;; 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
;;(test 0 (p))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))

;; 1.7
(define (sqrt-iter-precise guess x old-guess)
  (if (< (abs (- guess old-guess)) .00001)
      guess
      (sqrt-iter-precise
       (improve guess x)
       x
       guess)))

(define (sqrt-precise x)
  (sqrt-iter-precise 1.0 x x))
(square (sqrt-precise 40))



