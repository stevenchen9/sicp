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



