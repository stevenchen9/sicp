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

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))


;; these are correct, but unsimplified
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)



;; using a better "constructor" we can cause simplification
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))



;; sets as unordered
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
