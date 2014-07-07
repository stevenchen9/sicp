(require (planet neil/sicp:1:17))

;; The desired syntax for a nondeterministic langauge 

#|(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))|#

;; (list (amb 1 2 3) (amb 'a 'b))
;; => `(1 a)' `(1 b)' `(2 a)' `(2 b)' `(3 a)' `(3 b)'

(define (require? p)
  (if (not p) (amb) '()))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

