;; This simple withdraw, even if two concurrent systems
;; have access to the variable "amount", can have issues
;; when after testing the amount, someone else reduces it
;; enough to cause an issue

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficent funds"))


;; It is possible to restrict that no two operations
;; change any shared state variables


;; 3.38
;; 1 Peter: (set! balance (+ balance 10))
;; 2 Paul: (set! balance (- balance 20))
;; 3 Mary: (set! balance (- balance (/ balance 2)))

;; 1-2-3 = 100 +10 -20 -45 = 45 
;; 1-3-2 = 100 +10 -55 -20 = 35
;; 3-2-1 = 100 -50 -20 +10 = 40
;; 3-1-2 = 100 -50 +10 -20 = 40
;; 2-1-3 = 100 -20 +10 -45 = 45 
;; 2-3-1 = 100 -20 -40 +10 = 50


