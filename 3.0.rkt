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
