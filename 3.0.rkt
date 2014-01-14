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
      (cond ((not (eq? password pass-guess)) invalid-pass)
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
