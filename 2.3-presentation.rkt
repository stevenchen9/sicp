(define a 1)

(define b 2)

(list a b)

(list 'a 'b)

(list 'a b)

(car '(a b c))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))


;; Differentiation

;;   dc
;;   -- = 0  for c a constant, or a variable different from x
;;   dx

;;   dx
;;   -- = 1
;;   dx

;;   d(u + v)   du   dv
;;   -------- = -- + --
;;      dx      dx   dx

;;   d(uv)     / dv \     / du \
;;   ----- = u | -- | + v | -- |
;;    dx       \ dx /     \ dx /0

;;   (variable? e)          Is `e' a variable?
;;   (same-variable? v1 v2) Are `v1' and `v2' the same variable?
;;   (sum? e)               Is `e' a sum?
;;   (addend e)             Addend of the sum `e'.
;;   (augend e)             Augend of the sum `e'.
;;   (make-sum a1 a2)       Construct the sum of `a1' and `a2'.
;;   (product? e)           Is `e' a product?
;;   (multiplier e)         Multiplier of the product `e'.
;;   (multiplicand e)       Multiplicand of the product `e'.
;;   (make-product m1 m2)   Construct the product of `m1' and `m2'.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))
