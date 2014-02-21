;; propagation of contraints

;; This is a simple example of a framework for determining equations
;; based on a contraint system. You wire up the equation as a
;; set of either multipliers, constants, and adders, then solve
;; for any missing element.


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
