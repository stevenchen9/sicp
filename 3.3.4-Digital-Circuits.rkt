;; Digital Circuit example

;; a half-adder example api
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))

(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

;;(or-gate a b d)
;;(and-gate a b c)
;;(inverter c e)
;;(and-gate d e s)

;; a better half-adder api
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;; a full-adder using the half-adder "block"
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input inverter-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;; 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;; 3.29
;; or with "and" and "inverter"
;; the time delay for this will be the same as it is for
;; just the inverter then the and, same as it would be
;; in the regular circuit
(define (or-gate a1 a2 output)
  (let ((inv-to-and-wire (make-wire)))
    (inverter a1 inv-to-and-wire)
    (and-gate a1 inv-to-and-wire output)
    'ok))

;; 3.30

(define (ripple-carry-adder ak bk sk c)
  (define (ripple-inner as bs ss c-in)
    (if (pair? as)
        (let ((c-out (make-wire)))
          (full-adder (car as) (car bs) c-in (car ss) c-out)
          (ripple-inner (cdr as) (cdr bs) (cdr ss) c-out))
        'ok))
  (if (= (length ak) (length bk) (length sk))
      (ripple-inner ak bk sk c)
      (error "This needs an equal count of ak, bk, and sk")))


;; Wires
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal) set-my-signal!)
            ((eq? m 'add-action) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-val)
  ((wire 'set-signal) new-val))
(define (add-action! wire action)
  ((wire 'add-action) action))

(define w1 (make-wire))
(get-signal w1)
;; => 0
(set-signal! w1 1) ;; => done
(add-action! w1 (lambda () (print "called")))
(set-signal! w1 0)
;; => done  "called"



;; An agenda class for dealing with intervals
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-seconds the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define intverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)
(half-adder input-1 input-2 sum carry)


