;; Two main organizational strategies: object based and
;; stream based. Objects are hard because how can they change
;; and yet maintain their identity. Streams are most powerful
;; when one can decouple time from the order of events.

;; Using (set! <name> <new-value>) to mutate the
;; state of <name>, in this case to lower an account balance

;; Also, (begin <exp1> .... <expk>) evaluates the exp forms
;; in order, returning the last one.

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(withdraw 25)
;; => 75
(withdraw 25)
;; => 50 
(withdraw 60)
;; => "Insufficient funds"
(withdraw 15)
;; => 35

;; Rewriting withdraw to encapsulate the balance
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(new-withdraw 55)
;; => 45

;; A withdraw that returns a "stateful processor"
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))
  
(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
(W1 50)
;; => 50
(W2 70)
;; => 30
(W2 40)
;; => "Insufficient funds"
(W1 40)
;; => 10

;; A message passing style stateful closure that allows for a
;; dispatch to fully encapsulate and allow for external
;; mutation of its state 
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100))
((acc 'withdraw) 50)
;; => 50
((acc 'withdraw) 60)
;; => "Insufficient funds"
((acc 'deposit) 40)
;; => 90
((acc 'withdraw) 60)
;; => 30

;; 3.1 -- accumulator
(define (make-accumulator init)
  (lambda (to-add)
    (begin (set! init (+ init to-add))
           init)))

(define A (make-accumulator 5))
(A 10)
;; => 15 
(A 10)
;; => 25 

(define (make-monitored f)
  (let ((count 0))
    (define (dispatch m)
      (cond ((eq? m 'reset) (set! count 0))
            ((eq? m 'how-many-calls?) count)
            (else (begin
                    (set! count (+ 1 count))
                    (f m)))))
    dispatch))

(define s (make-monitored sqrt))
(s 100)
;; => 10
(s 25)
;; => 5 
(s 'how-many-calls?)
;; => 2
(s 'reset)
(s 'how-many-calls?)
;; => 0

;; 3.3 - Modified make-account
(define (make-account balance password)
  (let ((invalid-pass-count 0))
    (define (call-the-cops) "BAD BOYS, BAD BOYS, WHATCHA GUNNA DO?!!")
    (define (invalid-pass x)
      (if (= 6 invalid-pass-count)
          (call-the-cops)
          (begin
            (set! invalid-pass-count (+ 1 invalid-pass-count))
            "Incorrect password")))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch pass-guess m)
      (cond ((eq? m 'authorized) (eq? password pass-guess))
            ((not (eq? password pass-guess)) invalid-pass)
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define acc (make-account 100 'pass))
((acc 'pass 'withdraw) 50)
;; => 50
((acc 'wrong 'withdraw) 60)
;; => "Incorrect password"
((acc 'wrong 'withdraw) 60)
((acc 'wrong 'withdraw) 60)
((acc 'wrong 'withdraw) 60)
((acc 'wrong 'withdraw) 60)
((acc 'wrong 'withdraw) 60)
((acc 'wrong 'withdraw) 60)
;; => "BAD BOYS, BAD BOYS, WHATCHA GUNNA DO?!!"

;; 3.4 - add "call the cops" to new make-account
;; see above

;; Calls to a random function that require the previous
;; results of the run to execute
(define random-init 4)
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

;; Monte-carlo is a set of experiments that choose randomly
;; from a set, then making deductions based on the probabilities
;; of tabulating the results of those experiments

;; Using our stateful rand 
(define (estimiate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (cesaro-test)
  (= (gcd (* 100 (random)) (* 100 (random))) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
(estimiate-pi 3)

;; trying to use rand-update directly, and what a hassle
;; it is to have to store and pass x around, it is really
;; a break of encapsulation
;; Also, our awesome "monte-carlo" runner now has to
;; be implementation specific, due to the implementation
;; details of rand
(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))
(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))

;; while it might be possible to include an "experiment values"
;; parameter, it still is not a great design

;; 3.5
(define (estimate-integral P x1 x2 y1 y2 trials))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;; 3.6 - rand that resets
(define rand 
  (let ((x random-init))
    (lambda (action)
      (cond ((eq? action 'generate)
             (begin (set! x (rand-update x))
                    x))
            ((eq? action 'reset)
             (lambda (next)
               (set! x next)))))))

;; complicated version of make-withdraw with assignment
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))
(define W (make-simplified-withdraw 25))
(W 20)
;; => 5
(W 10)
;; => -5

;; make-decrementer does not store a state to mutate, it
;; is "pure" in that calling it with the same arguments
;; always returns the same values
(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))
(define D (make-decrementer 25))
(D 20)
;; => 5
(D 10)
;; => 15

;; Any make-decrementer value can be substitued for any other,
;; since they are "equal" in what they will return for a given
;; input. The opposite is true for make-withdraw values,
;; they are only sometimes equal. Therefore make-decrementer
;; is "referentially transparent". A function can be said to
;; be referentially transparent when it does not use assignment.

;; functional - no use of asssignment
;; imperitive - heavy use of assignment


;; functional factorial, no assignment
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; imperative factorial, uses assignment to overwrite the
;; values of product and counter each iteration
(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          ;; notice how if the two set!s were done in the
          ;; other order, the wrong result would be computed!
          ;; imperative introduces an implicit order where
          ;; true functional actually assigns both "simultaneously"
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))

;; 3.7 make-joint

(define (make-joint acc base-pass joint-pass)
  (define (dispatch pass-guess m)
    (cond ((not (eq? joint-pass pass-guess))
           (lambda (x) "Invalid password"))
          ((eq? m 'withdraw) (acc base-pass 'withdraw))
          ((eq? m 'deposit) (acc base-pass 'deposit))
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100 'pass))
(define jacc (make-joint acc 'pass 'jpass))
((jacc 'jpass 'withdraw) 50) 
;; => 50 
((jacc 'pass 'withdraw) 50) 
;; =>a "Invalid password" 

;; 3.8 - using assignment to "break" an ordered execution of calls

(define f 
  (let ((va 1))
    (lambda (x)
      (begin
        (set! va (- va x))
        va))))

(+ (f 0) (f 1)) ;; => 1  
(+ (f 1) (f 0)) ;;=> 0

;; 3.9
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
(factorial 6)
;; E1 n = 6
;; E2 n = 5
;; E3 n = 4
;; E4 n = 3
;; E5 n = 2
;; E6 n = 1

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
;; (factorial 6) E1 n = 6
;; E2 product = 1, counter = 1, max-count = 6
;; E3 product = 1, counter = 2, max-count = 6
;; E4 product = 2, counter = 3, max-count = 6
;; E5 product = 6, counter = 4, max-count = 6
;; E6 product = 24, counter = 5, max-count = 6
;; E7 product = 120, counter = 6, max-count = 6

;; 3.10

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))


;; internal definitions
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(sqrt 9)



;; Mutable Cons cells

(define (cons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))

;; 3.12

;; old immutable append 
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

;; new mutable append
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
;; (a b c d)
(cdr x)
;; b
(define w (append! x y))
w
;; (a b c d)
(cdr x)
;; (b c d)

;; 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
;; E1, x: (a b c d)
;;     y: ()
;; E2, x: (b c d)
;;     y: (a ())
;; E3, x: (c d)
;;     y: (b a ())
;; E3, x: (d)
;;     y: (c b a ())
;; E4, x: ()
;;     y: (d c b a ())
;; return y -> (d c b a ())

;; The "mystery" function reverses a list in a single pass
