* Chapter 1-2
** Homeworks

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

*** 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
   (* 3 (- 6 2) (- 2 7)))
*** 1.3
(define (sq x) (* x x))
(define (sum-of-squares2 x y z)
  (+ (sq x) (sq y) (sq z)))
(sum-of-squares2 1 2 3)
*** 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
*** 1.5
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

*** 1.7
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

*** 1.8
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

*** 1.10
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

*** 1.11
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

*** 1.12
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

*** 1.15
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

*** 1.17
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

*** 1.18
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

*** 1.19

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

*** 2.3
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

*** 2.4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))
(car (cons 1 2))
(cdr (cons 1 2))

*** 2.5
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

*** 2.6
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
***  2.7
(define (make-interval a b) (cons a b))

(define (upper-bound a) (cdr a))
(define (lower-bound a) (car a))

***  2.8

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

*** 2.17
(define (last-pair l)
  (define (last-iter il prev)
    (if (null? il)
        (list prev)
        (last-iter (cdr il) (car il))))
  (last-iter l null))

(last-pair (list 23 72 149 34))
;; => (34)

*** 2.18
;; Reverses a list
(define (reverse li)
  (if (= 1 (length li))
      li
      (append (reverse (cdr li)) (list (car li)))))

(reverse (list 1 4 9 16 25))
;; => (25 16 9 4 1)

***  2.19


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

***  2.20
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

***  2.21
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

***  2.22
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

***  2.23
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

***  2.24
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

***  2.25
(define x (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr x)))))
;; => 7
(car (cdaddr x))
;; => 7

***  2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
;; => (1 2 3 4 5 6)
(cons x y)
;; => ((1 2 3) 4 5 6)
(list x y)
;; => ((1 2 3) (4 5 6))

***  2.27 Deep Reverse
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


***  2.28
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

***  2.29
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

***  2.30
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

***  2.31
;; See above

***  2.32
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x) (cons (car s) x))
                     rest)))))
(subsets (list 1 2 3))

* Chapter 2
** 2.2.3 Sequences as Conventional Interfaces
*** Concepts
   By using the concept of a sequence, we can abstract over
   different concepts, like map, fold, accumulate, filter

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

*** 2.33
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

*** 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))
;; => 79

*** 2.25
;; Used the enumerate to "walk" the tree contents,
;; returning 1 for each element
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves (list x x))

*** 2.26
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define x (list
           (list 1 2 3)
           (list 4 5 6)
           (list 7 8 9)
           (list 10 11 12)))
(accumulate-n + 0 x)
;; => (22 26 30)

*** 2.27
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define m (list
           (list 1 2 3 4)
           (list 4 5 6 6)
           (list 6 7 8 9)))
(dot-product (first  m) (second  m))
;; => 56

*** 2.28
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define fold-right accumulate)

(fold-right / 1 (list 1 2 3))
;; => 3/2
(fold-left / 1 (list 1 2 3))
;; => 1/6
(fold-right list null (list 1 2 3))
;; => (1 (2 (3 ())))
(fold-left list null (list 1 2 3))
;; => (((() 1) 2) 3)

;; Produces the same value for any seq
(fold-right + 0 (list 1 2 3))
;; => 6
(fold-left + 0 (list 1 2 3))
;; => 6

*** 2.39
  (define (reverse sequence)
    (fold-right (lambda (next res)
                  (append res (list next)))
                null sequence))
  (reverse (list 1 2 3 4))
  ;; => (4 3 2 1)
  (define (reverse sequence)
    (fold-left (lambda (result next)
                 (append (list next) result))
               null sequence))
  (reverse (list 1 2 3 4))
  ;; => (4 3 2 1)

*** Nested Mappings
  (define (flatmap proc seq)
    (accumulate append null (map proc seq)))
  
  (define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))
  
  (define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
  
  (define (prime-sum-pairs n)
    (map make-pair-sum
         (filter prime-sum?
                 (flatmap
                  (lambda (i)
                    (map (lambda (j) (list i j))
                         (enumerate-interval 1 (- i 1))))
                  (enumerate-interval 1 n)))))
  
  (define (permutations s)
    (if (null? s)
        (list null)
        (flatmap (lambda (x)
                   (map (lambda (p) (cons x p))
                        (permutations (remove x s))))
                 s)))
  
  (define (remove item sequence)
    (filter (lambda (x) (not (= x item)))
            sequence))
  
  (permutations (list 1 2 3 4))
  (define (remove item sequence)
    (filter (lambda (x) (not (= x item)))
            sequence))
*** 2.40
*** 2.42
    
** 2.2.4 Example: A Picture Language

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(define wave4 (flipped-pairs wave))
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottem-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottem-right corner))))))
* 2.3 Symbolic Data
** 2.3.1 Quotation

Quoting is similar to what you would do when asking someone to
say their name: "say your name" => "steve", or, if you wanted
them to say those exact words, you would say,
"say 'your name'" => "your name". In this way, we "quote" to
force the interpreter to see it as a data object, not an
expression.

(define a 1)
(define b 2)
(list a b)
;; => (1 2) 

(list 'a 'b)
;; => (a b)

(car '(a b c))
;; => a

(cdr '(a b c))
;; => (b c) 

;; Finds the rest of the list starting with the first matching
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pair banana prune))
;; => #f
(memq 'apple '(pear (apple sauce) y apple pear))
;; => (apple pear)

*** 2.53
(list 'a 'b 'c)
;; => (a b c)
(list (list 'george))
;; => ((george))
(cdr '((x1 x2) (y1 y2)))
;; => ((y1 y2))
(cadr '((x1 x2) (y1 y2)))
;; => (y1 y2)
(pair? (car '(a short list)))
;; => #f
(memq 'red '((red shoes) (blue socks)))
;; => #f
(memq 'red '(red shoes blue socks))
;; => (red shoes blue socks)

*** 2.54
(equal? '(this is a list) '(this is a list))
;; => #t
(equal? '(this is a list) '(this (is a) list))
;; => #f

;; Reimplementing equal? in terms of eq? on symbols
(define (equal? l1 l2)
  (cond
   ((and (pair? l1) (pair? l2))
    (and (eq? (car l1) (car l2))
         (equal? (cdr l1) (cdr l2))))
   ((and (not (pair? l1))
         (not (pair? l2))) (eq? l1 l2))
   (else #f)))

(equal? 'a 'b)
;; => #f
(equal? 'a 'a)
;; => #t
(equal? '(test a) '(test a))
;; => #t
(equal? '(test a) '(test b))
;; => #f

*** 2.55
(car ''abricadabra)
;; => quote

The 'x is syntactual sugar over (quote x). By "expanding"
the double quote, we see something like (quote (quote x)).
Calling the car function to that returned list returns the
first element, the symbol 'quote.


** 2.3.1 Symbolic Differentiation
Making a differentiation function example

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; The question then becomes how to structure the data,
;; as the "api" has already been defined

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(deriv '(+ x 3) 'x)
;; => (+ 1 0)
(deriv '(* x y) 'x)
;; => (+ (* x 0) (* y 1))
(deriv '(* (* x y) (+ x 3)) 'x)
;; => (+ (* (* x y) (+ 1 0))
;;       (* (+ (* x 0) (* 1 y))
;;          (+ x 3)))

;; This works, but it is 'unsimplified' the results
;; have not been clarified.

;; (deriv '(* x y) 'x) => should be just y...

;; To do this we shouldn't need to change our derivation
;; algorithm at all, just the "constructor" of the
;; representation of the data.

(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        (else (list '* m1 m2))))

(deriv '(+ x 3) 'x)
;; => 1
(deriv '(* x y) 'x)
;; => y
(deriv '(* (* x y) (+ x 3)) 'x)
;; => (+ (* x y) (* y (+ x 3)))

;; The first two are fully simplified, but the last is not

*** 2.56
;; Including exponents in the deriv function
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        (else (list '** base exp))))

(define (deriv-exponent exp var)
  (make-product (exponent exp)
                (make-product
                 (make-exponentiation (base exp)
                                      (- (exponent exp) 1))
                 (deriv (base exp) var))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((exponentiation? exp) (deriv-exponent exp var))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(** x 1) 'x)
;; => 1
(deriv '(** x 0) 'x)
;; => 0
(deriv '(** x 2) 'x)
;; => (* 2 x) 
(deriv '(** x 3) 'x)
;; => (* 3 (** x 2))

*** 2.58
**** a
;; Making an infix set of selectors, e.g.: (1 + 1)
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))
(define (base x) (car x))
(define (exponent x) (caddr x))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        (else (list m1 '* m2))))
(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        (else (list base '** exp))))

(deriv '(x ** 1) 'x)
;; => 1
(deriv '(x ** 0) 'x)
;; => 0
(deriv '(x ** 2) 'x)
;; => (2 * x) 
(deriv '(x ** 3) 'x)
;; => (3 * (x ** 2))
(deriv '((x * y) * (x + 3)) 'x)
;; => ((x * y) + (y * (x + 3)))

**** b
;; Making a set of selectors for standard algebraic notation

;; Sum would be any list with a + and no *
(define (sum? x)
  (and (pair? x)
       (memq '+ x)
       (not (memq '* x))))
(define (product? x)
  (and (pair? x) (memq '* x)))
 
(define (addend s) (car s))
(define (augend s) (caddr s))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))
(define (base x) (car x))
(define (exponent x) (caddr x))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (all-before item li)
  (define (iter ret lis)
    (cond ((and (null? lis) ret
                (eq? item (car lis))) ret)
          (else (iter (append ret (list (car lis)))
                      (cdr lis)))))
  (iter '() li))
(all-before 3 '(1 2 3 4 5))

;; (deriv '(x + 3 * x + y + 2) 'x)

** 2.3.3 Example: Representing Sets

Here we use abstractions to define the set
data structure, again built on a cons cell linked
list, with functions to abstract the common
functionality

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; Filter out any not in both, the "AND"
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; (intersection-set '(1 2 3 4 5) '(3 4 5))  => (3 4 5)
;; (intersection-set '(1 2 3 4) '(1 2 3 7))  => (1 2 3)


*** 2.59 - Union Set

;; Show any in either, the "OR"
(define (union-set set1 set2)
  (cond ((null? set2) set1)
        (else (union-set
               (adjoin-set (car set2) set1)
               (cdr set2)))))

;; (union-set '(1 2 3 4) '(1 2 3 7 8 9))  => (9 8 7 1 2 3 4)

*** 2.60

;; This implementation of all sets is faster,
;; as adjoin no longer requires a traveral
;; therefore neither does union, but element
;; of set could be longer, as the size can grow
;; much faster
(define (adjoin-set x set) (cons x set))


*** Ordered Sets

;; An ordered set implementation allows for
;; an average existence check that is n/2 faster,
;; but still O(n)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

;; This intersection-set is much faster from the 
;; unordered implementation, this is only O(n)
;; since at most we only iterate through each
;; list once, rather than through set2
;; once for each element in set1, which is O(n^2), 
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

*** 2.61 - O(n) Adjoin-set

;; Traverse the list one by one until the
;; correct slot is found, then put the element
;; in as the first element of the cons cell,
;; with the "rest" in the second slot,
;; basically just a merge sort
(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set)
                               (adjoin-set x (cdr set))))))

;;(adjoin-set 5 '(1)) => (1 5)
;;(adjoin-set 5 '(1 2 3 4 9)) => (1 2 3 4 5 9)


*** 2.62 - O(n) Union-set

;; Due to the implicit sorted nature of the
;; two sets, we can remove the first element from
;; set1 if it is smaller than the first element
;; of set2, and viceversa, allowing for at most
;; a single traversal of either
(define (union-set set1 set2)
  (cond ((null? set1) '())
        ((null? set2) '())
        ((= (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1) (cdr set2))))
        ;; If one is not in the other, and it is already
        ;; "past", remove it 
        ((< (car set1) (car set2))
         (union-set (cdr set1) set2))
        ((> (car set1) (car set2))
         (union-set set1 (cdr set2)))))

;; (union-set '(1 2 3 4) '(1 2 3 7 8 9))  => (1 2 3)
;; (union-set '(2 3 4) '(1 2 3 7 8 9))  => (2 3)
;; (union-set '() '(1 2 3 7 8 9))  => ()
;; (union-set '(1 2 3 7 8 9) '())  => ()


*** Sets as Binary Trees
;; Trees, in any structure, will allow for a
;; speed up even over our O(n) algorithms, by
;; halving each time (assuming they are balanced)
;; the number of paths to search with each step,
;; reducing to O(log n)

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

*** 2.63 Convert binary tree to list

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define x (adjoin-set
    7 (adjoin-set
       2 (adjoin-set
          5 (adjoin-set
             3 (adjoin-set
                6 '()))))))
(tree->list-2 x)
;; => (2 3 5 6 7)

(tree->list-1 x)
;; => (2 3 5 6 7)

(define (m-tree elements)
  (if (null? elements)
      '()
      (adjoin-set (car elements)
                  (m-tree (cdr elements)))))

(define  fig2-16a (m-tree '(1 5 11 3 9 7))) 
;; => (11 (5 (1 () (3 () ())) (9 (7 () ()) ())) ())

(tree->list-1 fig2-16a)
;; => (1 3 5 7 9 11)
(tree->list-2 fig2-16a)
;; => (1 3 5 7 9 11)

(define fig2-16b (m-tree '(11 5 9 1 7 3))) 

(tree->list-1 fig2-16b)
;; => (1 3 5 7 9 11)
(tree->list-2 fig2-16b)
;; => (1 3 5 7 9 11)

(define fig2-16c (m-tree '(1 7 11 3 9 5))) 

(tree->list-1 fig2-16c)
;; => (1 3 5 7 9 11)
(tree->list-2 fig2-16c)
;; => (1 3 5 7 9 11)


*** 2.64

;; Since we assume the list is already sorted,
;; we can basically just split the list in half-1 each
;; time, making a tree out of the left and right sides,
;; then using the "pivot" as the root node.
;;
;; The left side is spit off from the list by
;; making a partial tree of the first half of the elements
;; and getting back that and the second half of the elements
;; as the return value. It stores the left-tree, and then
;; pulls the first of the unused elements as the "entry"
;; then takes the rest and then makes a second call to
;; partial-tree with just those elements. A new tree
;; is created and the remaining-elements are returned to be
;; used.
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;; The partial-tree function uses a single list to
;; return two different parts of state: the tree fragment
;; and the rest of the unused elemets.
;;
;; This gives the function the ability to simultaniously
;; increase one list while decreasing another, something
;; otherwise that would be difficult or impossible to
;; do with only one return value
;;
;; => (tree-fragment unused-elements)
(define (partial-tree elts n)
  (if (= n 0)
      ;; This has to return the remaining elements and
      ;; an empty list which will be used to fill in
      ;; an empty leaf node
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; Get list size 1


(list->tree '(1 3 5 7 9 11))
;; => (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
;;    5
;; 1    9
;;  3  7 11


*** 2.65 - Balanced tree intersection set

;; Just use the "tree->list" function
;; to O(n) convert the tree, then use the
;; regular O(n) intersection from earlier
(define  (intersection-set tree1 tree2)
  (define (intersection-inner set1 set2)
    (cond ((or (null? set1)
               (null? set2)) '())
          (else
           (let ((n1 (car set1))
                 (n2 (car set2)))
             (cond ((= n1 n2)
                    (cons n1
                          (intersection-inner (cdr set1)
                                              (cdr set2))))
                   ((< n1 n2)
                    (intersection-inner (cdr set1) set2))
                   ((> n1 n2)
                    (intersection-inner set1 (cdr set2))))))))
  (intersection-inner (tree->list-2 tree1)
                      (tree->list-2 tree2)))


(intersection-set (list->tree '(1 3 5 7 9 11))
                  (list->tree '(4 5 7)))
;; => (5 7)



