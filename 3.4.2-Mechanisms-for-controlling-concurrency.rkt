(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))

;; with interleaving, all these are possible:
;; 101 - p1, then p2
;; 121 - p2, then p1
;; 110 - interleaved..
;; 11 - interleaved..
;; 100 - interleaved..

;; (make-serializer <p>) will ensure no other process
;; can be interleavened with p's execution

;; 3.39
;; What are the possible results of this?
(define x 10)
(define s (make-serializer))
(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))a
                  (s (lambda () (set! x (+ x 1)))))

;; because the serializer will still prevent interleavening,
;; regardless of the lambda, only 101, and 121 will be options

;; 3.40
;; All possible values of x:
(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

;; a12->b123 = 10*10 = 100 * 100 * 100 = 1000000
;; b123->a12 = 10*10*10 = 1000 * 1000  = 1000000
;; a1->b123->a2 = 10 * (1000)          = 10000
;; b1->a12->b23 = 10 * (100) * (100)   = 100000
;; b12->a12->b3 = 10 * 10 * (100)      = 10000

(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))
;; a12->b123 = 10*10 = 100 * 100 * 100 = 1000000
;; b123->a12 = 10*10*10 = 1000 * 1000  = 1000000

;; 3.41
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) ((protected (lambda () balance))))
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))


;; This new make-account can prevent interleavening such as:
(define a (make-account 50))
(parallel-execute (lambda () ((a 'withdraw) 50))
                  (lambda () ((a 'withdraw) 50)))
;; Which could result in both checks to (>= balance amount)
;; happening at the same time, before the (set!) occurs

;; 3.42

