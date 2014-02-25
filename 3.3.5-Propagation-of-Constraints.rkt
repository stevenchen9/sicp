;; propagation of contraints

;; This is a simple example of a framework for determining equations
;; based on a contraint system. You wire up the equation as a
;; set of either multipliers, constants, and adders, then solve
;; for any missing element.


(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints))
          'false)
      (if (has-value? me)
          (inform-about-value new-constraint)
          'false)
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))




;; Here is a representation of temp conversion using:
;; 9C = 5(F - 32)

;; Since 9, 5, and 32 are constants, we give them "connectors" to
;; those values

(define C (make-connector))
(define F (make-connector))
(celsuis-fahrenheit-converter C F)

(define (celsuis-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))


(probe "C temp" C)
(probe "F temp" F)

(set-value! C 25 'user)
;; Probe: C temp = 25
;; Probe: F temp = 77
(forget-value! C 'user)

(set-value! F 212 'user)
;; Probe: F temp = 212
;; Probe: C temp = 100

;; 3.33
;; To produce a "divide" we
;; can either reduce or create a new
;; function that allows for division by
;; simply swapping the order of elements

;; Reducing to just add/mult
;; (a + b) / 2 = c
;; 2 c = a + b 

;; Creating a "divider" using multiply
;; a / b = c
;; a = b * c
(define (divider a b c)
  (multiplier b c a))
(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(divider a b c)
(constant 10 a)
(constant 2 b)
(get-value c)
;; => 5


;; (a + b) / 2 = c
(define (averager a b c)
  (let ((to-count (make-connector))
        (add-out (make-connector)))
    (constant 2 to-count)
    (adder a b add-out)
    (divider add-out to-count c)
    'ok))

(begin 
  (define a (make-connector))
  (define b (make-connector))
  (define c (make-connector))
  (probe "A" a)
  (probe "B" b)
  (probe "C" c)
  (averager a b c))
(set-value! a 5 'a)
(set-value! b 10 'b)
;; Probe: C = 15/2
;; Probe: B = 10


;; re-running "setup"
(set-value! c 10 'c)
(set-value! b 5 'b)
;; Probe: A = 15
;; Probe: B = 5

;; 3.34
;; This is not possible because a cannot set itself, it would
;; loop forever, and because if a and b are set: then a
;; is set too...
(define (squarer a b)
  (multiplier a a b))

;; 3.35
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (* (get-value a) (get-value a)) me)
            'ok)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))
  (connect a me)
  (connect b me)
  me)

(begin 
  (define a (make-connector))
  (define b (make-connector))
  (probe "A" a)
  (probe "B" b)
  (squarer a b))
(set-value! a 5 'a)
;; Probe: B = 25
;; Probe: A = 5
(set-value! b 25 'a)
;; Probe: A = 5
;; Probe: B = 25



