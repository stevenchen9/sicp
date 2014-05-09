


;; "Similarly, as program designers"
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))










;; Common
(define (<NAME> a b)
  (if (> a b)
      0
      (+ (<TERM> a)
         (<NAME> (<NEXT> a) b))))






(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))











(define (cube n) (* n n n))
(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10) 
;; => 3025



(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)
;; => 55









;; Lambdas
(lambda (x)
  (/ 1.0 (* x (+ x 2))))










;; Sugaring
(define (adder x)
  (+ x x))

;; Define (define (<procedure> <args..>) <body>)
;; is just "syntatic sugar" for:
(define adder (lambda (x)
                (+ x x)))

(define adder (+ 1 1))











;; Convert an inner procedure 
(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))







;; ...To a lambda
(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))








;; ...To a let
(define (f x y)
  (let [(a (+ 1 (* x y)))
        (b (- 1 y))]
    (+ (* x (square a))
       (* y b)
       (* a b))))







;; Which reads easier?
;; The define/lambda form has an implicit "jump" associated with it













;; The "shadowing" of a value
(let [(x 4)]
  (+ 3 x))









;; "Shadowing"
(define (do x)
  (+ (let [(x 3)] x) 
   x)) 
(do 5)













;; Returned lambdas

;; Takes a procedure -> returns a procedure
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define damped-square (average-damp square))
(damped-square 10)
;; => 55











(((lambda (x)
    (lambda (x)
      x))
  50)
 25)








(((lambda () +)) 50 50)





(let [(x 50)]
  (let [(x 25)]
    x))





(let [(x 50)]
  ((lambda (x) x)
   x))







((lambda (x)
     ((lambda (x) x) x))
 50)









(define inner (lambda (x) x))
(define outer (lambda (x) (inner x)))
(outer 50)


(define (inner x) x)
(define (outer x) (inner x))
(outer 50)








;; What makes something "first class"?
;; * They may be named by variables.
;; * They may be passed as arguments to procedures.
;; * They may be returned as the results of procedures.
;; * They may be included in data structures.(7)
;; Therefore, in scheme/racket, procedures are "first class"
