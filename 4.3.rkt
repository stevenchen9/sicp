(require (planet neil/sicp:1:17))

;; The desired syntax for a nondeterministic langauge 

#|(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))|#

(list (amb 1 2 3) (amb 'a 'b))
;; => `(1 a)' `(1 b)' `(2 a)' `(2 b)' `(3 a)' `(3 b)'

(define (require? p)
  (if (not p) (amb) '()))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;; 4.36
(define (an-integer-between n m)
  (amb n (when (<= n m) (an-integer-between (+ n 1) m))))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require? (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

;; 4.37
;; Ben Bitdiddle's more efficient solution, less results are
;; required to be enumerated
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))
