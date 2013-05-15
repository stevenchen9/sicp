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

;;1.8
(define (cube x)
  (* x x x))

(define (improve-cube-guess y x)
  (/ (+ (/ x (square y)) (* 2 y)) 3))
(define (cube-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-iter (improve-cube-guess guess x) x)))
(define (cube-good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))
(define (cube-root x)
  (cube-iter 1.0 x))
(cube-root 27)

;; block/lexical scoping
(define (sqrt-lex x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(sqrt-lex 9)



