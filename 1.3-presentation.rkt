;; "Similarly, as program designers"

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)
55









;; Lambdas
(lambda (x)
  (/ 1.0 (* x (+ x 2))))


(define (adder x)
  (+ x x))

;; Define (define (<procedure> <args..>) <body>)
;; is just "syntatic sugar" for:
(define adder (lambda (x)
                (+ x x)))















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
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; Which reads easier?
;; The define/lambda form has an implicit "jump" associated with it

;; Let is just "syntactic sugar" over lambda

;; The "shadowing" of a value











(let ((x 4))
  (+ 3 x))




(+
 (let ((x 3)) (+ x (* x 10))) ;; x = 3
 x) ;; x = 5

;; The x is shadowed inside the let, but then "unshadowed" outside











;; Returned lambdas

;; Takes a procedure -> returns a procedure
(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)
;; => 55




















;; What makes something "first class"?
;; * They may be named by variables.
;; * They may be passed as arguments to procedures.
;; * They may be returned as the results of procedures.
;; * They may be included in data structures.(7)

;; Therefore, in scheme/racket, procedures are "first class"
