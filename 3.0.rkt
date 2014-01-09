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
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

;; Monte-carlo is a set of experiments that choose randomly
;; from a set, then making deductions based on the probabilities
;; of tabulating the results of those experiments

(define (estimiate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
