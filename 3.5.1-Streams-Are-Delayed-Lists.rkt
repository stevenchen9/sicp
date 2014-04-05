
;; Two ways of defining the count of primes in a range

;; Iteratively
(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b accum))
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

;; As sequence abstractions
(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

;; The second, while more elegant is very inefficient
;; because it enumerates the entire list of integers
;; before filtering down only primes


;; A "stream" is basically just a lazy eval'ed list

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons (proc (stream-car s))
            (delay (stream-map proc (stream-cdr s))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))


;; We can delay the filling of the cdr of a stream like so:

(delay <A>) ;;returns a promise, that can be "forced" with (force)

;; These are equivalent
(cons-stream <A> <B>) ;; special form
(cons <A> (delay <B>))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(stream-car
 (stream-cdr
  (stream-filter even?
                 (stream-enumerate-interval 10000 1000000))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons
       low
       (delay (stream-enumerate-interval (+ low 1) high)))))

;; In this example, the "subs" model shows how it is just
;; as efficient as an iterative solution
(cons 10000
      (delay (stream-enumerate-interval 10001 1000000)))

;; The cdr of the list hasn't been evaluated yet, it is a
;; promise to evaluate the "rest" of the intervals later

;; stream-filter will lazily one at a time
;; leaving the cdr as the unevaluated recursive call.
;; If it doesn't find any sutable match for pred,
;; it will traverse the whole list searching for one
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons (stream-car stream)
               (delay (stream-filter pred
                                     (stream-cdr stream)))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-null? s)
  (null? s))

;; Delay and force as simple concepts

(delay <EXP>)
;; could be rewritten
(lambda () <EXP>)
;; and force...
(define (force s) (s))

;; But this can cause many times the same forcing to happen on the
;; same object, which is inefficient

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

;; delay must be defined such that these are equivalent
(delay <EXP>)
(memo-proc (lambda () <EXP>))
;; and `force` can stay the same
(define the-empty-stream '())

;; 3.50 - Generalized stream-map
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons
        (apply proc (map stream-car argstreams))
        (delay (apply stream-map
                      (cons proc (map stream-cdr argstreams)))))))

(display-stream (stream-map + (stream-enumerate-interval 1 10) (stream-enumerate-interval 1 10)))
;; =>  2 4 6 8 10 12 14 16 18 20

;; 3.51
(define (show x)
  (display-line x)
  x)
(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
;; => 1 2 3 4 5
(stream-ref x 7)
;; => 6 7

;; 3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
;; => sum = 6
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
;; => sum = 10 

(stream-ref y 7)
;; => sum = 136 
(display-stream z)
;; => sum = 210
;; => done 10 15 45 55 105 120 190 210


;; This is an infinite stream of integers
(define (integers-starting-from n)
  (cons n (delay (integers-starting-from (+ n 1)))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(stream-ref no-sevens 100)
;; => 117

(define (fibgen a b)
  (cons a (delay (fibgen b (+ a b)))))

(define fibs (fibgen 0 1))

;; this makes an infinite stream of primes, because
;; each prime has to be not divisible by all previous
;; numbers
(define (sieve stream)
  (cons
   (stream-car stream)
   (delay
     (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream))))))
(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 50)
;; => 233


;; An infinite stream of 1's
(define ones (cons 1 (delay ones)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

;; keep adding the first elements of integers and ones
;; making a stream of all numbers
(define integers (cons 1 (delay (add-streams ones integers))))
(stream-ref integers 10)
;; => 11

(define fibs
  (cons 0
        (delay
          (cons 1
                (delay (add-stream (stream-cdr fibs)
                                   fibs))))))



(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons 1 (delay (scale-stream double 2))))
(stream-ref double 10)
;; => 1024


;; A recursive definition that relies on the primes stream
;; to check if a number is prime? which is needed to filter
;; for the primes stream which needs the prime? function to
;; have access to itself to.... HUUUUURRRRRKK
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define primes
  (cons
   2
   (delay (stream-filter prime? (integers-starting-from 3)))))


;; *Exercise 3.53:* Without running the program, describe the
;; elements of the stream defined by
(define s (cons 1 (delay (add-streams s s))))

;; This generates a stream that has the powers of two: 2, 4, 8, 16 

;; *Exercise 3.54:* Define a procedure `mul-streams', analogous to
;; `add-streams', that produces the elementwise product of its two
;; input streams.  Use this together with the stream of `integers' to
;; complete the following definition of the stream whose nth element
;; (counting from 0) is n + 1 factorial:

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define ones (cons 1 (delay ones)))
(define integers (cons 1 (delay (add-streams ones integers))))

;; This list of factorials works by mulitplying each number n
;; by the equivalent number of factorials, so when n = 3
;; we are mulitiplying that by factorial(2) = 2 so the result
;; is 6
(define factorials
  (cons 1
        (delay (mul-streams integers factorials))))

(stream-ref factorials 2)
;; => 2
(stream-ref factorials 4)
;; => 24


;; *Exercise 3.55:* Define a procedure `partial-sums' that takes as
;; argument a stream S and returns the stream whose elements are S_0,
;; S_0 + S_1, S_0 + S_1 + S_2, ....  For example, `(partial-sums
;; integers)' should be the stream 1, 3, 6, 10, 15, ....

(define (partial-sums s)
  (define f
    (cons 0
        (delay (add-streams s f))))
  f)

(define t (partial-sums integers))
(stream-ref t 4)
;; => 10
(stream-ref t 5)
;; => 15


;; *Exercise 3.56:* A famous problem, first raised by R. Hamming, is
;; to enumerate, in ascending order with no repetitions, all positive
;; integers with no prime factors other than 2, 3, or 5.  One obvious
;; way to do this is to simply test each integer in turn to see
;; whether it has any factors other than 2, 3, and 5.  But this is
;; very inefficient, since, as the integers get larger, fewer and
;; fewer of them fit the requirement.  As an alternative, let us call
;; the required stream of numbers `S' and notice the following facts
;; about it.
  
;;    * `S' begins with 1.
  
;;    * The elements of `(scale-stream S 2)' are also elements of `S'.
  
;;    * The same is true for `(scale-stream S 3)' and `(scale-stream
;;      5 S)'.
  
;;    * These are all the elements of `S'.
  
;; Now all we have to do is combine elements from these sources.  For
;; this we define a procedure `merge' that combines two ordered
;; streams into one ordered result stream, eliminating repetitions:
  
;; Then the required stream may be constructed with `merge', as
;; follows:
  
;;      (define S (cons-stream 1 (merge <??> <??>)))
  
;; Fill in the missing expressions in the places marked <??> above.
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons s1car (delay (merge (stream-cdr s1) s2))))
                 ((> s1car s2car)
                  (cons s2car (delay (merge s1 (stream-cdr s2)))))
                 (else
                  (cons s1car
                        (delay (merge (stream-cdr s1)
                                      (stream-cdr s2))))))))))

;; (require (planet neil/sicp:1:17))

(define (take S n)
  (define (take-i s counter ret)
    (if (= n counter)
        (reverse ret)
        (take-i (stream-cdr s)
                (+ 1 counter)
                (cons (stream-car s) ret))))
  (take-i S 0 '()))

(load "3.5.1-Support.rkt")
(define S (cons 1 (delay (merge (merge (scale-stream S 2) (scale-stream S 3))
                                (scale-stream S 5)))))
(take S 10)
;; => {1 2 3 4 5 6 8 9 10 12}


;; 3.5.3 - Exploiting streams

(load "3.5.1-Support.rkt")
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons 1.0
          (delay (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses))))
  guesses)

;; (display-stream (sqrt-stream 2))
;; =>   (1.  1.5   1.4166666666666665  1.4142156862745097 1.4142135623746899 ...)


(define (pi-summands n)
  (cons (/ 1.0 n)
        (delay (stream-map - (pi-summands (+ n 2))))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(take pi-stream 5)
;; (0 4.0 2.666666666666667 3.466666666666667 2.8952380952380956)


;; Here we take the less approximate pi-stream and create a new stream
;; out of the first three numbers in pi-stream. Then it repeats creating
;; the next value out of the 2nd-4th numbers, etc. In this way, we use several
;; of the pi-stream numbers to create a new stream of values.
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; S_(n-1)
        (s1 (stream-ref s 1))           ; S_n
        (s2 (stream-ref s 2)))          ; S_(n+1)
    (cons (- s2 (/ (square (- s2 s1))
                   (+ s0 (* -2 s1) s2)))
          (delay (euler-transform (stream-cdr s))))))

(take (euler-transform pi-stream) 5)
;; => (3.0 3.166666666666667 3.1333333333333337 3.1452380952380956 3.13968253968254) 


;; We can accelerate our euler stream transformer...
(define (make-tableau transform s)
  (cons s
        (delay (make-tableau transform
                             (transform s)))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(take (accelerated-sequence euler-transform pi-stream) 5)
;; => (0 3.0 3.1388888888888893 3.1415610507386664 3.141592402966356)


;;   *Exercise 3.63:* Louis Reasoner asks why the `sqrt-stream'
;;   procedure was not written in the following more straightforward
;;   way, without the local variable `guesses':

;;        (define (sqrt-stream x)
;;          (cons-stream 1.0
;;                       (stream-map (lambda (guess)
;;                                     (sqrt-improve guess x))
;;                                   (sqrt-stream x))))

;;   Alyssa P. Hacker replies that this version of the procedure is
;;   considerably less efficient because it performs redundant
;;   computation.  Explain Alyssa's answer.  Would the two versions
;;   still differ in efficiency if our implementation of `delay' used
;;   only `(lambda () <EXP>)' without using the optimization provided
;;   by `memo-proc' (section *Note 3-5-1::)?

;; The recursive call to (sqrt-stream x) will cause a large slowdown,
;; unless using memo-proc



;;   *Exercise 3.64:* Write a procedure `stream-limit' that takes as
;;   arguments a stream and a number (the tolerance).  It should
;;   examine the stream until it finds two successive elements that
;;   differ in absolute value by less than the tolerance, and return
;;   the second of the two elements.  Using this, we could compute
;;   square roots up to a given tolerance by

(define (stream-limit s tol)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1)))
    (if (> tol (abs (- s0 s1)))
        s1
        (stream-limit (stream-cdr s) tol))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 40 12)

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons (stream-car s1)
            (delay (interleave s2 (stream-cdr s1))))))

(define (pairs s t)
  (cons
   (list (stream-car s) (stream-car t))
   (delay (interleave
           (stream-map (lambda (x) (list (stream-car s) x))
                       (stream-cdr t))
           (pairs (stream-cdr s) (stream-cdr t))))))


(take (pairs integers integers) 10)
;; => ((1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (1 5) (2 4) (1 6))



;; *Exercise 3.67:* Modify the `pairs' procedure so that `(pairs
;; integers integers)' will produce the stream of _all_ pairs of
;; integers (i,j) (without the condition i <= j).  Hint: You will
;; need to mix in an additional stream.

(define (all-pairs s t)
  (cons
   (list (stream-car s) (stream-car t))
   (delay (interleave
           (stream-map (lambda (x)
                         (list (stream-car s) x))
                       (stream-cdr t))
           (interleave (stream-map (lambda (x)
                                     (list x (stream-car t)))
                                   (stream-cdr s))
                       (pairs (stream-cdr s) (stream-cdr t)))))))

(take (all-pairs integers integers) 20)
;; => ((1 1) (1 2) (2 1) (1 3) (2 2) (1 4) (3 1) (1 5) (2 3) (1 6) (4 1) (1 7) (3 3) (1 8) (5 1) (1 9) (2 4) (1 10) (6 1) (1 11))


