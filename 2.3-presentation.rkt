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



;; sets - unordered

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 3 '(2 4 1 7))
(element-of-set? 3 '(2 4 1 3 7))
;; Requires theta(n) traversals



(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(adjoin-set 4 '(3))
;; also theta(n)


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(intersection-set '(1 2 4) '(2 3 4))
;; theta(n^2) .. for every element, iterate on the other once

(time (intersection-set (range 1 10000 3) (range 2 10000 1)))
;; > cpu time: 1591 real time: 1590 gc time: 0

(time (intersection-set (range 1 10000 2) (range 2 10000 2)))
;; > cpu time: 2414 real time: 2412 gc time: 0

;; by making them ordered, we can shorten that

;; sets - ordered

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 5 '(6 7 88))

;; doesn't change theta(n) but shortens the average by a factor of 2


(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(time (intersection-set (range 1 10000 3) (range 2 10000 1)))
;; > cpu time: 2 real time: 2 gc time: 0
(time (intersection-set (range 1 10000 2) (range 2 10000 2)))
;; > cpu time: 2 real time: 2 gc time: 0

;; at most steps is the sum of the sizes of the two elements, or theta(n)

;; sets - binary trees

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define x (adjoin-set
    7 (adjoin-set
       2 (adjoin-set
          5 (adjoin-set
             3 (adjoin-set
                6 '()))))))
(tree->list-2 x)
;; => (2 3 5 6 7)

(tree->list-1 x)
;; => (2 3 5 6 7)

(define (m-tree elements)
  (if (null? elements)
      '()
      (adjoin-set (car elements)
                  (m-tree (cdr elements)))))

(define  fig2-16a (m-tree '(1 5 11 3 9 7))) 
;; => (11 (5 (1 () (3 () ())) (9 (7 () ()) ())) ())


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      ;; This has to return the remaining elements and
      ;; an empty list which will be used to fill in
      ;; an empty leaf node
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; Get list size 1


(list->tree '(1 3 5 7 9 11))
;; => (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
;;    5
;; 1    9
;;  3  7 11


;; Just use the "tree->list" function
;; to O(n) convert the tree, then use the
;; regular O(n) intersection from earlier
(define  (intersection-set tree1 tree2)
  (define (intersection-inner set1 set2)
    (cond ((or (null? set1)
               (null? set2)) '())
          (else
           (let ((n1 (car set1))
                 (n2 (car set2)))
             (cond ((= n1 n2)
                    (cons n1
                          (intersection-inner (cdr set1)
                                              (cdr set2))))
                   ((< n1 n2)
                    (intersection-inner (cdr set1) set2))
                   ((> n1 n2)
                    (intersection-inner set1 (cdr set2))))))))
  (list->tree (intersection-inner (tree->list-2 tree1)
                                  (tree->list-2 tree2))))


(intersection-set (list->tree '(1 3 5 7 9 11))
                  (list->tree '(4 5 7)))
;; => (5 () (7 () ()))
(time (intersection-set (list->tree (range 1 10000 3))
                        (list->tree (range 2 10000 1))))
;; cpu time: 14 real time: 15 gc time: 5

