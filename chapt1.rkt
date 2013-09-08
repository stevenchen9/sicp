(define (assert-equal x y)
  (if (equal? x y)
      #t
      (string-append "failed: expected " (number->string y) " was "  (number->string x))))

(define (tests a . b)
  (display (foldl (lambda (x acum)
                    (if (equal? true x)
                        (string-append acum "p\n")
                        (string-append acum  x " \n")))
                  ""
                  (cons a b))))

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
        ((< x 0) (* -1 x))))
(define (abs2 x)
  (cond ((< x 0) (* -1 -x))
        (else x)))
(define (abs3 x)
  (if (< x 0)
      (* -1 x)
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
(define (average3 x y z)
  (/ (+ x y z) 3))
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

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
(factorial 4) ;24

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(factorial 40)

;; 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(A 1 10) ;;  1024
(A 2 4) ;; 65536
(A 3 3) ;; 65536

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
(fib 6)

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 100)

;; 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (f n)
  (fi 2 1 0 n))
(define (fi n-1 n-2 n-3 count)
  (if (= count 0)
      n-3
      (fi (+ n-1 (* 2 n-2) (* 3 n-3))
          n-1 n-2 (- count 1))))
(f 5) ;; 25
(f 10) ;;1892 
(f 12) ;;1892 

;; 1.12
;;    1  
;;   1 1  
;;  1 2 1  
;; 1 3 3 1
;;1 4 6 4 1
(define (pascal-triangle r c)
  (if (= r 1)
      (if (or (> 1 c) (< r c)) 0 1) ;; add "0" around the outsides
      (+  (pascal-triangle (- r 1) (- c 1))
          (pascal-triangle (- r 1) c))))

(tests (assert-equal (pascal-triangle 2 1) 1)
       (assert-equal (pascal-triangle 3 2) 2)
       (assert-equal (pascal-triangle 5 3) 6)
       (assert-equal (pascal-triangle 1 1) 1))

;; 1.15
(define (cube x) (* x x x))
(define (p x)
  (print "x")
  (- (* 3 x) ( * 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))
(sine 12.15) ; p -> 5 times
(sine 30) ; p -> 6 times
(sine 100) ; p -> 7 times

;; recursive exponent
(define (expt-rec b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
(expt-rec 5 5)

;; iterative exponent
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

(define (expt b n)
  (expt-iter b n 1))
(expt 5 5)

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(fast-expt 40 50)

;; 1.17
(define (double x)
  (* 2 x))
(define (halve x)
  (/ x 2))
(halve 4)

(define (fast-mult x y)
  (if (= y 0)
      0
      (if (even? y)
          (double (fast-mult x (halve y)))
          (+ x (double (fast-mult x (halve (- y 1))))))))

(tests (assert-equal (fast-mult 5 5) 25)
       (assert-equal (fast-mult 5 4) 20))

;; 1.18
(define (mult-i x y)
  (mult-iter x y 0))
(define (mult-iter x y product)
  (if (= y 0)
      product
      (if (even? y)
          (mult-iter (double x)
                     (halve y)
                     product)
          (mult-iter x
                     (- y 1)
                     (+ x product)))))

(tests (assert-equal (mult-i 5 5) 25)
       (assert-equal (mult-i 5 4) 20))

;; 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
(fib 6)

;; Euclids algorithm for greatest common divisor

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(gcd 206 40) ;; 2

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
(prime? 4)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 5 100)

;1.21
(smallest-divisor 199) ;199
(smallest-divisor 1999) ;1999
(smallest-divisor 19999) ;7

;1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      ""))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;1.23
(define (next-divisor n)
  (if (= n 2)
      3
      (+ n 2)))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(smallest-divisor 19999)       

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))


(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))
(* 8 (pi-sum 1 1000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

;1.29
(define (simpsons-rule f a b n)
  (define (inc n) (+ n 1))
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((odd? k) 4)
             ((or (= k 0) (= k n)) 1)
             ((even? k) 2))
       (y k)))
  (/ (* h (sum term 0 inc n)) 3))

(simpsons-rule cube 0 1 0.01)

;1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (sum-cubes a b)
  (sum cube a inc b))
(sum-cubes 1 10) ; 3025

;1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (no-change x) x)
(define (factorial n)
  (product no-change 1 inc n))
(factorial 4) ;24

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (sum-cubes a b)
  (sum cube a inc b))
(sum-cubes 1 10) ; 3025

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;1.33
(define (filtered-accumulate combiner pred null-value term a next b)
  (if (> a b)
      null-value
      (if (pred a)
          (combiner (term a)
                    (filtered-accumulate combiner pred null-value term (next a) next b))
          (filtered-accumulate combiner pred null-value term (next a) next b))))

(define (sum-prime a b)
  (filtered-accumulate + prime? 0 square a inc b))
(sum-prime 1 10) ; a. 88
(define (prod-prime a b)
  (filtered-accumulate * prime? 1 identity a inc b))
(prod-prime 1 10)


(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

((lambda (x y z) (+ x y (square z))) 1 2 3)

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
       (* y b)
       (* a b)))
   (+ 1 (* x y))
   (- 1 y)))
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;1.34
(define (f g)
  (g 2))

(f square)
(f (lambda (z) (* z (+ z 1))))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)


(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(sqrt 9)

(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))
(cube-root 27)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (cube x) (* x x x))
((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))
(sqrt 9)

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))
;1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x)
            (* a x x)
            (* b x)
            c)))
(newtons-method (cubic 3 -2.4 6) 1)

;1.41
(define (double func)
  (lambda (x) (func (func x))))

(((double (double double)) inc) 5) ;21

;1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6) ;49

;1.43
(define (repeated func count)
  (if (= count 1)
      func
      (compose func (repeated func (- count 1)))))
((repeated square 2) 5) ;625

;1.44
(define (f x) (cos x))
(define (smooth f dx)
  (lambda (x)
    (average3 (f (- x dx))
              (f x)
              (f (+ x dx)))))
((smooth f 1.0) 5)
(define (smooth-n f dx n)
  (repeated (smooth f dx) n))
((smooth-n f 1.0 100) 5)

(define (linear-combination a b x y)
  (+ (* a x) (* b y)))
(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat  (* (numer x) (numer y))
             (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat  (* (numer x) (denom y))
             (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third)) ;6/9

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))


(print-rat (add-rat one-third one-third)) ;2/3

;2.1
(define (make-pos x) (invert-if x (negative? x)))
(define (make-neg x) (invert-if x (positive? x)))
(define (invert-if x pred)
  (if pred
      (* -1 x)
      x))
(define (make-rat n d)
  (if (negative? (/ n d))
      (cons (make-neg n) (make-pos d))
      (cons (make-pos n) (make-pos d))))
(make-rat -1 2) ;-1/2
(make-rat -1 -2) ;1/2
(make-rat 1 -2) ;-1/2
(make-rat 1 2) ;1/2

; perform the gcd check on each lookup..
(define (make-rat n d)
  (cons n d))
(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))
(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

;2.2
(define (make-point x y)
  (cons x y))
(define (x-point x)
  (car x))
(define (y-point x)
  (cdr x))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(print-point (make-point 1 3))
(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))
(define (midpoint-segment seg)
  (make-point
   (average (x-point (start-segment seg))
            (x-point (end-segment seg)))
   (average (y-point (start-segment seg))
            (y-point (end-segment seg)))))
(define p1 (make-point 3 3))
(define p2 (make-point 6 6))
(define seg1 (make-segment p1 p2))
(print-point (midpoint-segment seg1))

;;2.3
(define (make-rect p1 p2 p3 p4)
  (list p1 p2 p3 p4))
(define (seg-length p1 p2)
  ;; a = x2-x1,b = y2-y1 
  ;; a^2 + b^2 = c^2
  (sqrt (+ (abs (- (x-point p1) (x-point p2)))
           (abs (- (y-point p1) (y-point p2))))))

(seg-length (make-point 1 1) (make-point 0 0))
(seg-length (make-point 1 0) (make-point 0 0)) ;1

(define (p1 rect) (car rect))
(define (p2 rect) (cadr rect))
(define (p3 rect) (caddr rect))
(define (p4 rect) (cadddr rect))
(define (rect-perimeter rect)
  (+ (seg-length (p1 rect) (p2 rect))
     (seg-length (p2 rect) (p3 rect))
     (seg-length (p3 rect) (p4 rect))
     (seg-length (p4 rect) (p1 rect))))

(define rect1 (make-rect
               (make-point 0 0)
               (make-point 1 0)
               (make-point 1 1)
               (make-point 0 1)))
(rect-perimeter rect1) ;;4.0


(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

;;2.4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))
(car (cons 1 2))
(cdr (cons 1 2))

;;2.5
(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (times-divisible num div)
  (define (inner x count)
    (if (= (remainder x div) 0)
        (inner (/ x div) (+ count 1))
        count))
  (inner num 0))

(define (car x) (times-divisible x 2))
(define (cdr x) (times-divisible x 3))

(define pair (cons 3 4))
(car pair) ;;3
(cdr pair) ;;4

;;2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (inc c)
  (+ c 1))

((zero inc) 0)
((zero inc) 1)

(define one (add-1 zero))
(define two (add-1 one))

(define one
  (lambda (f) (lambda (x) (f x))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))

((one inc) 0) ;; 1
((one inc) 5) ;; 6
((two inc) 3) ;; 5

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (add-church m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(define three (add-church one two))
((three inc) 0) ;; 3


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
;; 2.7
(define (make-interval a b) (cons a b))

(define (upper-bound a) (cdr a))
(define (lower-bound a) (car a))

;; 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))


;; Simple cons cell list of numbers
(cons 1 (cons 2 (cons 3 (cons 4 null))))
;; is the same as
(list 1 2 3 4)

(define one-through-four (list 1 2 3 4))
(car one-through-four)
(cdr one-through-four)
(car (cdr one-through-four))
(cons 10 one-through-four)
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(list-ref squares 3) ;;16

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))
(length odds) ;;4

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(append squares odds)

(append odds squares)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;;2.17
(define (last-pair l)
  (define (last-iter il prev)
    (if (null? il)
        (list prev)
        (last-iter (cdr il) (car il))))
  (last-iter l null))

(last-pair (list 23 72 149 34))
;; => (34)

;;2.18
;; Reverses a list
(define (reverse li)
  (if (= 1 (length li))
      li
      (append (reverse (cdr li)) (list (car li)))))

(reverse (list 1 4 9 16 25))
;; => (25 16 9 4 1)

;; 2.19


(define (count-change amount)
  (cc amount us-coins))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values))
                coin-values)))))

(define (except-first-denomination li)
   (cdr li))
(define (no-more? li)
  (= 0 (length li)))
(define (first-denomination li)
  (car li))
(cc 100 us-coins)
(cc 100 uk-coins)

;; 2.20
;; A . in a parameter list will put all parameters after into a list
(define (same-parity first . rest)
  (cons first
        (if (odd? first)
            (filter odd? rest)
            (filter even? rest))))
(same-parity 1 2 3 4 5 6 7 8)
;; => (1 3 5 7)
(same-parity 2 3 4 5 6 7 8)
;; => (2 4 6 8)


;; Mapping

;; A scaling example that doesn't use map
(define (scale-list items factor)
  (if (null? items)
      null
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))
(scale-list (list 1 2 3 4 5) 10)

;; An example of map
(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))
(map abs (list -10 2.5 -11.6 17))
;; => (10 2.5 11.6 17)

(map (lambda (x) (* x x))
     (list 1 2 3 4))
;; => (1 4 9 16)

;; using the new map abstraction
(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(scale-list (list 1 2 3) 2)
;; => (2 4 6)

;; 2.21
(define (square-list items)
  (if (null? items)
      null
      (cons (* (car items) (car items))
            (square-list (cdr items)))))

(square-list (list 1 2 3 4))
;; => (1 4 9 16)

(define (square-list items)
  (map (lambda (x) (* x x))
       items))
(square-list (list 1 2 3 4))
;; => (1 4 9 16)

;; 2.22
(define (square x)
  (* x x))
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))

(square-list (list 1 2 3 4))
;; => (16 9 4 1)

;; reversing the cons order
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))

(square-list (list 1 2 3 4))
;; => ((((() . 1) . 4) . 9) . 16)

;; The second list is incorrect because it is building the
;; cons cells backwards. All that is needed to fix the first
;; list is to reverse the items before starting the iteration,
;; what is odd is that it builds the list from the inside out,
;; starting with the last cons cell, and consing on each
;; subsequent argument. 

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter (reverse items) null))

(square-list (list 1 2 3 4))
;; => (1 4 9 16)

;; 2.23
(define (for-each items fun)
  (map items fun)
  null)
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
(cons (list 1 2) (list 3 4)) 

;; Tree data structures
(define x (cons (list 1 2) (list 3 4)))
;; Incorrect counts of leaves
(length x) ;; => 3
(length (list x x)) ;;=> 2

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; Correct counts of leaves
(count-leaves x) ;; => 4
(count-leaves (list x x)) ;; => 8

;; 2.24
(list 1 (list 2 (list 3 4)))
;; => (1 (2 (3 4)))

;; Tree diagram
;;  (1 (2 (3 4)))
;;   /     \
;; 1    (2 (3 4))
;;      /     \
;;     2     (3 4)
;;          /    \
;;         3      4 

;; 2.25
(define x (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr x)))))
;; => 7
(car (cdaddr x))
;; => 7

;; 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
;; => (1 2 3 4 5 6)
(cons x y)
;; => ((1 2 3) 4 5 6)
(list x y)
;; => ((1 2 3) (4 5 6))

;; 2.27 Deep Reverse
(define x (list (list 1 2) (list 3 4) (list 5 6)))
(reverse x)
;; => ((3 4) (1 2))
(define (deep-reverse li)
  (define (reverse-if-list items)
    (if (pair? items)
        (deep-reverse items)
        items))
  (if (= 1 (length li))
      (list (if (pair? (car li))
                (deep-reverse (car li))
                (car li)))
      (append (deep-reverse (cdr li))
              (list (reverse-if-list (car li))))))
(deep-reverse x)
;; => ((6 5) (4 3) (2 1))


;; 2.28
(define x (list (list 1 2) (list 3 4)))

;; ((1 2) (3 4))
;;    /     \
;; (1 2)   (3 4)
;;  /  \    /  \
;; 1   2   3   4
(define (fringe items)
  (cond ((null? items) null)
        ((not (pair? items)) (list items))
        (else (append (fringe (car items))
                      (fringe (cdr items))))))

(fringe (list x x))
;; => (1 2 3 4 1 2 3 4)

;; 2.29
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))
(define (left-branch tree)
  (car tree))
(define (right-branch tree)
  (cadr tree))
(define (branch-length tree)
  (car tree))
(define (branch-structure tree)
  (cadr tree))
(define (weigh branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))
(define (total-weight mobile)
  (+ (weigh (left-branch mobile))
     (weigh (right-branch mobile))))
(define (is-balanced? mobile)
  (= (weigh (left-branch mobile))
     (weigh (right-branch mobile))))

(define mob (make-mobile (make-branch 0 1) (make-branch 0 1)))
(define mob1
  (make-mobile
   (make-branch 1 mob)
   (make-branch 1 mob)))
(total-weight mob1)
;; => 4 
(is-balanced? mob1)
;; => #t

;; The two "selector" functions just have to change to support
;; the change to the structure to use cons instead of list


;; Mapping over trees
;; Iteratively
(define (scale-tree tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)

;; Using map
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;; 2.30
;; iteratively
(define (walk-tree tree func)
  (cond ((null? tree) null)
        ((not (pair? tree)) (func tree))
        (else (cons (walk-tree (car tree) func)
                    (walk-tree (cdr tree) func)))))
(define (square-tree tree)
  (walk-tree tree (lambda (x) (square x))))

(define t (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree t) 
;; => (1 (4 (9 16) 25) (36 49))

;; Recursively, with map
(define (walk-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (walk-tree sub-tree factor)
             (factor sub-tree)))
       tree))

(define (square-tree tree)
  (walk-tree tree (lambda (x) (square x))))

(square-tree t) 
;; => (1 (4 (9 16) 25) (36 49))

;; 2.31
;; See above

;; 2.32
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x) (cons (car s) x))
                     rest)))))
(subsets (list 1 2 3))

;; 2.2.3 Seqs as interfaces

;; Both of these functions do: Enumerate, Filter, Map, Accumulate
;; 1. Enumerate -> tree-leaves
;; 2. Filter -> odd?
;; 3. Map -> square
;; 4. Accumulate -> +
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

;; 1. Enumerate -> integers
;; 2. Map -> fib
;; 3. Filter -> even?
;; 4. Accumulate -> cons
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        null
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))
(even-fibs 10)


;; Trying to decompose these two
;; similar flows into sequence abstractions

;; A map
(map square (list 1 2 3 4 5))
;; => (1 4 9 16 25)

;; A filter operation
(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(filter odd? (list 1 2 3 4 5))
;; => (1 3 5)

;; An acumulation operation
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))
;; => 15
(accumulate cons null (list 1 2 3 4 5))
;; => (1 2 3 4 5)

;; An incrementing enumerator
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)
;;=> (2 3 4 5 6 7)

;; An tree walking operation
(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
;; => (1 2 3 4 5)

;; Redefining the two earlier functions as seq operations
(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
              null
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

;; splitting out some of the even-fibs function
(define (list-fib-squares n)
  (accumulate cons
              null
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))
(list-fib-squares 10)
;; => (0 1 1 4 9 25 64 169 441 1156 3025)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))
(product-of-squares-of-odd-elements (list 1 2 3 4 5))
;; => 225


;; 2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              null
              sequence))
(map (lambda (x) (+ 1 x)) (list 1 2 3))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(append (list 1 2) (list 3 4))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(length (list 1 2 3 4 5))






